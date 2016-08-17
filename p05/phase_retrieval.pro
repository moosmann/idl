function paganin, img, beta=beta, delta=delta, camera=camera, propagation_distance=propagation_distance, energy=energy, binning=binning, pix_size=pix_size

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

  x_vec=2*!PI/pix_size*(indgen(T(1))-T(1)/2)/T(1)
  y_vec=2*!PI/pix_size*(indgen(T(2))-T(2)/2)/T(2)
  r_field=1.*(x_vec^2#(make_array(T(1))+1)+(make_array(T(1))+1)#y_vec^2)

  corr=alog(fft(fft(image, /center)/(1+(!PI*lambda*propagation_distance*delta/(4*!const.pi*beta))*r_field), /center, /inverse))

  return, real_part(corr(size_diff(1)/2:size_diff(1)/2+S(1)-1,size_diff(2)/2:size_diff(2)/2+S(2)-1))

end

function ctf, img, beta=beta, delta=delta, camera=camera, propagation_distance=propagation_distance, energy=energy, binning=binning, pix_size=pix_size

  img=reform(img)

  nan_list= where(finite(img) eq 0 )
  print, 'nan_list:', nan_list
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

  x_vec=2*!PI/pix_size*(indgen(T(1))-T(1)/2)/T(1)
  y_vec=2*!PI/pix_size*(indgen(T(2))-T(2)/2)/T(2)
  r_field=1.*(x_vec^2#(make_array(T(1))+1)+(make_array(T(1))+1)#y_vec^2)

  corr=alog(fft(fft(image, /center)/(1+(!PI*lambda*propagation_distance*delta/(4*!const.pi*beta))*r_field), /center, /inverse))

  return, real_part(corr(size_diff(1)/2:size_diff(1)/2+S(1)-1,size_diff(2)/2:size_diff(2)/2+S(2)-1))

end