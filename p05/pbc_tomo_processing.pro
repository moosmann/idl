function pbc_retrieval, img, beta=beta, delta=delta, camera=camera, propagation_distance=propagation_distance, energy=energy, binning=binning, pix_size=pix_size

  img=reform(img)

  nan_list= where(finite(img) eq 0 )
  foreach element, nan_list do begin
    if element eq 0 or element eq n_elements(img)-1 then img(element)=0 else img(element)=mean([img(element-1),img(element+1)],/nan)
  endforeach
  S=size(img)

  lambda=!const.c*!const.h/!const.e/energy

  ; image padding

  n_margin=ceil(3.*lambda*propagation_distance/pix_size^2)
  pre_padded_img=[S(1)+2*n_margin,S(2)+2*n_margin]
  padded_img=make_array(exp(alog(2)*ceil(alog2(pre_padded_img(0)))),exp(alog(2)*ceil(alog2(pre_padded_img(1)))))
  padded_delta=make_array(exp(alog(2)*ceil(alog2(pre_padded_img(0)))),exp(alog(2)*ceil(alog2(pre_padded_img(1)))))
  padded_beta=make_array(exp(alog(2)*ceil(alog2(pre_padded_img(0)))),exp(alog(2)*ceil(alog2(pre_padded_img(1)))))
  T=size(padded_img)
  size_diff=T-S

  padded_img(size_diff(1)/2:size_diff(1)/2+S(1)-1,size_diff(2)/2:size_diff(2)/2+S(2)-1)=img
  padded_img(0:size_diff(1)/2-1,size_diff(2)/2:size_diff(2)/2+S(2)-1)=img(0,*)##(0*indgen(size_diff(1)/2)+1)
  padded_img(size_diff(1)/2+S(1):*,size_diff(2)/2:size_diff(2)/2+S(2)-1)=img(-1,*)##(0*indgen((size_diff(1)+1)/2)+1)
  padded_img(*,0:size_diff(2)/2-1)=padded_img(*,size_diff(2)/2)#(0*indgen(size_diff(2)/2)+1)
  padded_img(*,size_diff(2)/2+S(2):*)=padded_img(*,size_diff(2)/2+S(2)-1)#(0*indgen((size_diff(2)+1)/2)+1)
  image=padded_img
  stop
  x_vec=2*!PI/pix_size*(indgen(T(1))-T(1)/2)/T(1)
  y_vec=2*!PI/pix_size*(indgen(T(2))-T(2)/2)/T(2)
  r_field=1.*(x_vec^2#(make_array(T(1))+1)+(make_array(T(1))+1)#y_vec^2)

  corr=alog(fft(fft(image, /center)/(1+(!PI*lambda*propagation_distance*delta/(4*!const.pi*beta))*r_field), /center, /inverse))

  return, real_part(corr(size_diff(1)/2:size_diff(1)/2+S(1)-1,size_diff(2)/2:size_diff(2)/2+S(2)-1))

end


pro pbc_tomo_processing, data_dir, energy, distance, pixel_size, rotation=rotation, no_projections=no_projections,rec_center=rec_center,binning=binning

if n_elements(binning) eq 0 then binning=2
num_processes=30
child_existance_check
if n_elements(rotation) eq 0 then rotation=!PI
if rotation gt 100 then rotation=rotation/180*!PI
if keyword_set(no_projections) eq 0 then ATT_CORRELATION, data_dir, num_processes=num_processes,binning=binning
binning=1
temp=strsplit(data_dir, '/raw/', /extract,/regex)
flat_corrected_dir=temp(0)+'/processed/'+temp(1)+'/flat_corrected/'
save_dir=temp(0)+'/processed/'+temp(1)+'/pbc/'
reco_dir=temp(0)+'/processed/'+temp(1)+'/pbc_reco/'
file_mkdir,save_dir
file_mkdir,reco_dir
start=systime(/seconds)
print, 'Reading attenuation data'
  projections=file_search(flat_corrected_dir+'*.tif')
  temp=read_tiff(projections(0))
  as=size(temp(*,*))
  att_volume=make_array(as(1),as(2),n_elements(projections)-1)

  for i=0,n_elements(projections)-2 do begin
  temp=read_tiff(projections(i))
  att_volume[*,*,i]=temp(*,*)
  end
  S=size(att_volume)/binning
  pbc_volume=make_array(S(1),S(2),S(3)*binning)
  
  for i=0, num_processes-1 do begin
    if i eq 0 then child=obj_new("IDL_IDLBridge", output='') else child=[child, obj_new("IDL_IDLBridge", output='')]
    child[i]->execute, "CD, '/asap3/petra3/gpfs/common/p05/DPC'"
    child[i]->setvar, 'distance', distance
    child[i]->setvar, 'energy', energy
    child[i]->setvar, 'binning', binning

    child[i]->setvar, 'pixel_size', pixel_size
    child[i]->execute, "@dpc_install"
   endfor
  percent=0
 
  for i=0, S(3)*binning-1+num_processes do begin
 
  child_num=i mod num_processes
  repeat begin status=child[child_num]->status(Error=error)
endrep until status eq 0

if i ge num_processes then begin
  percent_old=percent
  percent=100L*(i-num_processes)/(S(3)*binning)
  if percent ne percent_old then dpc_statusbar, percent
  
    catch, error

    if error ne 0 then begin
      print, 'error detected'
      child[child_num]->execute, "@dpc_install"
      child[child_num]->setvar, 'img',att_volume(*,*,i-num_processes)
      child[child_num]->execute, "pbc=float(pbc_retrieval(img, beta=10, delta=100, camera='EHD', propagation_distance=distance, energy=energy, binning=binning, pix_size=pixel_size))"
      catch, /cancel
    endif
    pbc_volume(*,*,i-num_processes)=-child[child_num]->getvar('pbc')
    temp_mean=mean([pbc_volume(0:50,*,i-num_processes),pbc_volume(-50:*,*,i-num_processes)])
    pbc_volume(*,*,i-num_processes)=pbc_volume(*,*,i-num_processes)-temp_mean
  endif

  if i ge S(3)*binning then goto, loopend
  if i mod 10 eq 0 then print, 'Processing Projection: '+string(i)+'+'
  child[child_num]->setvar, 'img',att_volume(*,*,i)
  child[child_num]->execute, "pbc=float(pbc_retrieval(img, beta=10, delta=100, camera='EHD', propagation_distance=distance, energy=energy, binning=binning, pix_size=pixel_size))", /nowait
  print, i, child_num
  loopend:

   
endfor

  child_kill, child

for i=0, S(3)*binning-1 do begin
write_tiff, save_dir+'/pbc_'+string(format='(I04)',i)+'.tif', pbc_volume(*,*,i), /float
endfor

if n_elements(rec_center) eq 0 then rec_center=att_find_rec_center(1-pbc_volume(*,s(2)/2,*), rotation=rotation, 30, off_axes=0, preview=preview)

for i=0, num_processes-1 do begin
  if i eq 0 then child=obj_new("IDL_IDLBridge", output='') else child=[child, obj_new("IDL_IDLBridge", output='')]
  child[i]->execute, "CD, '/asap3/petra3/gpfs/common/p05/DPC'"
  child[i]->setvar, 'rotation', rotation
  child[i]->execute, "@dpc_install"
endfor
percent=0
for i=0, S(2)-1+num_processes do begin
  
  child_num=i mod num_processes
  repeat begin status=child[child_num]->status(Error=error)
endrep until status eq 0

if i ge num_processes then begin
  percent_old=percent
  percent=100L*(i-num_processes)/(S(2))
  if percent ne percent_old then dpc_statusbar, percent

  catch, error

  if error ne 0 then begin
    print, 'error detected'
    child[child_num]->execute, "@dpc_install"
    child[child_num]->setvar, 'sino',1-pbc_volume(*,i-num_processes,*)
    child[child_num]->setvar, 'center',rec_center
    child[child_num]->execute, "reco=att_fbp_quick(sino, center, rot=rotation, off_axes=0)"
    catch, /cancel
  endif
  reco=child[child_num]->getvar('reco')

  write_tiff, reco_dir+'/reco_'+string(format='(I04)',i-num_processes)+'.tif', reco, /float
endif

if i ge S(2) then goto, loopend2
if i mod 10 eq 0 then print, 'Reconstructing Slice: '+string(i)+'+'
child[child_num]->setvar, 'sino',1-pbc_volume(*,i,*)
child[child_num]->setvar, 'center',rec_center
child[child_num]->execute, "reco=att_fbp_quick(sino, center, rot=rotation, off_axes=0)", /nowait

loopend2:

endfor
  

  child_kill, child



finish=systime(/seconds)
print, 'Finished after '+string(format='(F8.3)',finish-start)+'seconds'
end

