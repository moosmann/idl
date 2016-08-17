function Norm_CalcAbsImg, nr, rawbin=rawbin, x0=x0, x1=x1, $
  ref=ref, img=img, imagerot = imagerot, silent = silent
  common reco_petra_common, scaninfo, recoinfo

  if n_elements(rawbin) eq 0    then _rawbin = 2          else _rawbin = rawbin
  if n_elements(numangles) eq 0 then _numangles = 900     else _numangles = numangles
  if n_elements(x0) eq 0        then _x0 = 0              else _x0 = x0
  if n_elements(x1) eq 0        then _x1 = 3055 / _rawbin else _x1 = x1
  if n_elements(imagerot) eq 0  then _imagerot = 0        else _imagerot = imagerot
  if n_elements(silent) eq 0    then _silent = 0          else _silent = silent

  if recon_abs_query('scanmode') eq '180deg' then angleDelta = 180. / _numangles
  if recon_abs_query('scanmode') eq '360deg' then angleDelta = 360. / _numangles

  xsize=scaninfo.img_xmax-scaninfo.img_xmin+1
  ysize=scaninfo.img_ymax-scaninfo.img_ymin+1
  stimg=uintarr(xsize,ysize,scaninfo.n_img)
  nbasic=scaninfo.scan_rawbasepath+path_sep()+scaninfo.scan_beamtime+path_sep()+scaninfo.scan_path+path_sep()
  wimg=where(scaninfo.stimg_type eq 1)

  ;if n_elements(w) ne 1 then stop
  index=reform(wimg(nr))
  avcurrentImg=reform(scaninfo.stimg_avcurrent[index[0]])
  avcurrentRef=reform(scaninfo.stimg_avcurrent[scaninfo.stimg_correfind(index)])
  if _rawbin gt 1 then begin
    img=bin((read_dat(nbasic+scaninfo.stimg_name[index])-recoinfo.dark)/avcurrentImg[0],_rawbin)
    ref=bin((read_dat(nbasic+scaninfo.stimg_name[scaninfo.stimg_correfind(index)])-recoinfo.dark)/avcurrentRef[0],_rawbin)
  endif else begin
    img=(read_dat(nbasic+scaninfo.stimg_name[index])-recoinfo.dark)/avcurrentImg[0]
    ref=(read_dat(nbasic+scaninfo.stimg_name[scaninfo.stimg_correfind(index)])-recoinfo.dark)/avcurrentRef[0]
  endelse
  if not _silent then print,'Winkel',scaninfo.stimg_angle(index)

  abso=-alog(img/ref)
  ;abso=(img/ref); calculating transmission not absorption for paganin
  if _imagerot ne 0 then abso = rot(abso, 180 / !pi * _imagerot, /interp)
  return,abso
end