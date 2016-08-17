pro Corr_Prepare_RefImages, x00 = x00, x01 = x01, x10 = x10, x11 = x11, bin = bin, y0=y0, y1=y1, _scanmode=scanmode
  ; Corr_Prepare_RefImages useage

  common reco_petra_common, scaninfo, recoinfo
 
  if n_elements(bin) eq 0 then bin = 1
  if n_elements(x00) eq 0 then x00 = 0 / bin
  if n_elements(x01) eq 0 then x01 = 100 / bin
  if n_elements(x10) eq 0 then x10 = 2955 /bin
  if n_elements(x11) eq 0 then x11 = 3055 / bin
  if n_elements(scanmode) eq 0          then  _scanmode = '180deg' else _scanmode = scanmode 
  
  xsize=scaninfo.img_xmax-scaninfo.img_xmin+1
  ysize=scaninfo.img_ymax-scaninfo.img_ymin+1
  stdark=uintarr(xsize,ysize,scaninfo.n_dark)
  stimg=uintarr(xsize,ysize,scaninfo.n_img)
  ;dark
  w=where(scaninfo.stimg_type eq -1)
  ;if n_elements(w) ne scaninfo.n_dark then stop
  
  nbasic=scaninfo.scan_rawbasepath+path_sep()+scaninfo.scan_beamtime+path_sep()+scaninfo.scan_path+path_sep()
  
  print,'dark creation'
  wait,.1
  for i=0,scaninfo.n_dark-1 do stdark(*,*,i)=read_dat(nbasic+scaninfo.stimg_name(w(i)),/silent)
  dark=removehits(stdark)
  
  w=where(scaninfo.stimg_type eq 0)
  if w(0) eq -1 then stop
  refco=n_elements(w)
  print,'refstack creation (',strtrim(refco,2),')'
  coin=[indgen(x01 - x00) + x00, indgen(x11- x10) + x10]
  if _scanmode eq '360deg' then begin
    print,'scanmode "360deg" selected'
    coin=[indgen(x01 - x00)]
    recon_abs_set,'correlation area left start',x00
    recon_abs_set,'correlation area left end',x01
   endif else begin
    recon_abs_set,'correlation area left start',x00
    recon_abs_set,'correlation area left end',x01
    recon_abs_set,'correlation area right start',x10
    recon_abs_set,'correlation area right end',x11
  endelse
  stavref=fltarr(n_elements(coin),ysize,refco)
  to=0d
  avreffactor=1.
  for j=0,refco-1 do begin
    index=w(j)
    avcu=scaninfo.stimg_avcurrent[index]
    ref0=read_dat(nbasic+scaninfo.stimg_name[index],/silent)-dark
    if avcu[0] eq 0 then begin
      print, 'correction of avcurrent for ref ',index
      scaninfo.stimg_avcurrent[index]=mean(ref0)*avreffactor
    endif else avreffactor=avcu[0]/mean(ref0)
    ref=ref0/avcu[0]
    stavref(*,*,j)=ref(coin,*)
    to+=total(ref, 1)
  endfor
  w=where(to ge max(to)/8)
  
  if n_elements(y0) ne 0 then begin
    print, 'set slrangemin to: ',y0
    recon_abs_set,'slrangemin',y0
  endif else begin
    print, 'set slrangemin to: ',min(w)/bin
    recon_abs_set,'slrangemin',min(w)/bin ; for rawbin = 2
    print,min(w),max(w)
  endelse
  
  if n_elements(y1) ne 0 then begin
    print, 'set slrangemax to: ',y1
    recon_abs_set,'slrangemax',y1
  endif else begin
    print, 'set slrangemax to: ',max(w)/bin
    recon_abs_set,'slrangemax',max(w)/bin
    print,min(w),max(w)
  endelse
  
  wait,.1
  recoinfo=create_struct('dark',dark,'stavref',stavref,'stavrefco',refco,'coin',coin)
end


pro Corr_CalcCorrelation, loadcorrelation = loadcorrelation
  common reco_petra_common, scaninfo, recoinfo
  if n_elements(loadcorrelation) eq 0 then loadcorrelation = 0
  
  wimg=where(scaninfo.stimg_type eq 1)
  stcor=fltarr(recoinfo.stavrefco)
  stcor1=fltarr(recoinfo.stavrefco)
  stcor2=fltarr(recoinfo.stavrefco)
  nbasic=scaninfo.scan_rawbasepath+path_sep()+scaninfo.scan_beamtime+path_sep()+scaninfo.scan_path+path_sep()
  wref=where(scaninfo.stimg_type eq 0)
  avimgfactor=1
  for i=0,n_elements(wimg)-1 do begin
    index=wimg(i)
    img0=read_dat(nbasic+scaninfo.stimg_name[index],/silent)-recoinfo.dark
    avcurrent=reform(scaninfo.stimg_avcurrent[index])
    img0mean = mean(img0)
    if avcurrent[0] eq 0 then begin
      print, 'correction of avcurrent for img ',index
      avcurrent=img0mean*avimgfactor
      scaninfo.stimg_avcurrent[index]=avcurrent
    endif else avimgfactor=avcurrent[0]/img0mean
    img=img0/avcurrent[0]
    
    if loadcorrelation eq 0 then begin
      for j=0,recoinfo.stavrefco-1 do begin
        stcor(j)=correlate(img(recoinfo.coin,*),recoinfo.stavref(*,*,j))
      ;stcor1(j)=correlate(img(recoinfo.coin,*),reform(recoinfo.stavref(*,*,j)),/cov)
      ;stcor2(j)=-total((img(recoinfo.coin,*)-reform(recoinfo.stavref(*,*,j)))^2)
      endfor
    endif
    w=where(stcor eq max(stcor))
    w1=where(stcor1 eq max(stcor1))
    w2=where(stcor2 eq max(stcor2))
    scaninfo.stimg_correfind(index)=wref(w(0))
    print,i,index," ",scaninfo.stimg_name(index)," ",scaninfo.stimg_correfind(index),' ',scaninfo.stimg_name(scaninfo.stimg_correfind(index)),w[0],w1[0],w2[0]
    wait,.05 
    img0 = 0
    img = 0
    heap_gc
  endfor
  if loadcorrelation eq 1 then begin 
    restore, scaninfo.scan_name + '_scaninfo_with_corr.sav', /relaxed_structure_assignment
  endif
save, scaninfo, file= scaninfo.scan_recopath + '/' + scaninfo.scan_name +'_scaninfo_with_corr.sav'
save, recoinfo, file= scaninfo.scan_recopath + '/' + scaninfo.scan_name +'_recoinfo_with_corr.sav'
end
