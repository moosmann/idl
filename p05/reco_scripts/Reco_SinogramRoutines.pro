function Sino_CalcAbsImg, nr, rawbin=rawbin, x0=x0, x1=x1, numangles = numangles, $
  ref=ref, img=img, imagerot = imagerot, silent = silent
  common reco_petra_common, scaninfo, recoinfo

  if n_elements(rawbin) eq 0    then _rawbin = 2          else _rawbin = rawbin
  if n_elements(numangles) eq 0 then _numangles = 900     else _numangles = numangles
  if n_elements(x0) eq 0        then _x0 = 0              else _x0 = x0
  if n_elements(x1) eq 0        then _x1 = (scaninfo.img_xmax - scaninfo.img_xmin) / _rawbin else _x1 = x1
  if n_elements(imagerot) eq 0  then _imagerot = 0        else _imagerot = imagerot
  if n_elements(silent) eq 0    then _silent = 0          else _silent = silent

  if recon_abs_query('scanmode') eq '180deg' then angleDelta = 180. / _numangles
  if recon_abs_query('scanmode') eq '360deg' then angleDelta = 360. / _numangles

  winkel=0+ nr* angleDelta
  xsize=scaninfo.img_xmax-scaninfo.img_xmin+1
  ysize=scaninfo.img_ymax-scaninfo.img_ymin+1
  stimg=uintarr(xsize,ysize,scaninfo.n_img)
  nbasic=scaninfo.scan_rawbasepath+path_sep()+scaninfo.scan_beamtime+path_sep()+scaninfo.scan_path+path_sep()
  wimg=where(scaninfo.stimg_type eq 1)
  wih=where(scaninfo.stimg_angle(wimg) ge (winkel-angleDelta/10))
  w=where(scaninfo.stimg_angle(wimg(wih)) le winkel+angleDelta/10)
  if w(0) eq -1 then stop
  ;if n_elements(w) ne 1 then stop
  index=reform(wimg(wih(w(n_elements(w)-1))))
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
  return,abso[_x0:_x1, *]
end

pro Sino_SinogramCreation, rawbin = rawbin, x0 = x0, x1 = x1, numangles = numangles, st = st, imagerot = imagerot
  common reco_petra_common, scaninfo, recoinfo
  if n_elements(rawbin) eq 0    then _rawbin=FIX(recon_abs_query('rawbin')) $
                                else _rawbin = rawbin
  if n_elements(numangles) eq 0 then _numangles = 900      else _numangles = numangles
  if n_elements(x0) eq 0        then _x0 = 0               else _x0 = x0
  if n_elements(x1) eq 0        then _x1 = 3055 / rawbin   else _x1 = x1 
  if n_elements(imagerot) eq 0  then _imagerot = 0         else _imagerot = imagerot

  ymin=fix(recon_abs_query('ymin'))
  ymax=fix(recon_abs_query('ymax'))
  sinobin=FIX(recon_abs_query('sinobin'))
  yanz=((ymax-ymin)+1)/sinobin /rawbin *2
  ;recon_abs_set,'slrangemin',0
  ;recon_abs_set,'slrangemax',fix((ymax-ymin+1)/rawbin/sinobin *2)-1
  slicerange=[fix(recon_abs_query('slrangemin')),fix(recon_abs_query('slrangemax'))]
  
  if n_elements(st) eq 0 then begin
    st=fltarr(_x1-_x0 +1,ymax-ymin+1,_numangles)
    for i=0,(_numangles-1) do st(*,*,i)=Sino_CalcAbsImg(i, rawbin = _rawbin, x0 = _x0 , x1 = _x1, numangles = _numangles, imagerot = _imagerot)
  endif
  
  s=fltarr(_x1-_x0 +2,_numangles)
  if recon_abs_query('scanmode') eq '180deg' then $
      s[0,*]=findgen(_numangles)* (180. / _numangles) $
  else $
      s[0,*]=findgen(_numangles)* (360. / _numangles)
      
  slrangemin=fix(recon_abs_query('slrangemin'))
  slrangemax=fix(recon_abs_query('slrangemax'))
  n=recon_abs_query('recopfad')+'/sino/'+recon_abs_query('scanname')
  for z=ymin,ymax do begin
      s[1:(_x1-_x0+1),*]=st(*,z,*)
       write_dat,s,n+numstr(z),'sis',dummy
  endfor
  
; for z=slrangemin,slrangemax do begin
;    s[0:(_x1-_x0),*]=st(*,z,*)
;    write_dat,s,n+numstr(z),'sis',dummy
;  endfor
end

FUNCTION Sino_FilterRaw, data, thresh=thresh, max_repl_lines=max_repl_lines
  IF N_ELEMENTS(thresh) EQ 0 THEN STOP
  IF N_ELEMENTS(max_repl_lines) EQ 0 THEN max_repl_lines=200

  dat=data
  dat=filter1(dat, -1, rx=4, ry=0)

  s=SIZE(dat)
  nan=SQRT(-1)

  mask1D=REPLICATE(FIX(0), s(1))

  smdat=dat-SMOOTH(dat, 2, /nan)
  FOR i=0, s(1)-1 DO IF MEAN(smdat(i,*), /nan) GT thresh THEN mask1D(i)=1
  PRINT, 'replace lines first level threshhold: ',TOTAL(mask1d)
  mask2D=CONGRID(REFORM(mask1D, s(1), 1), s(1), s(2))
  w=WHERE(mask2D EQ 1)
  IF w(0) NE -1 THEN dat(w)=nan

  thresh2=thresh*.75
  no_changes=0
  REPEAT BEGIN
    mask1D_old=mask1D

    ;determine surrounding of rad_sur arround selected pixels
    rad_sur=2
    mask1D_sur=CONVOL(mask1D, REPLICATE(FIX(1),2*rad_sur+1))

    w=WHERE(mask1D_sur GE 1)
    IF w(0) NE -1 THEN mask1D_sur(w)=1
    w=WHERE(mask1D EQ 1)
    IF w(0) NE -1 THEN mask1D_sur(w)=0

    ;Find increased values in this surrounding. Use SMOOTH over many pixels
    w=WHERE(ABS(TOTAL(dat-SMOOTH(dat, 6, /nan), 2, /nan)/s(2))*mask1D_sur  GT thresh2)

    IF w(0) NE -1 THEN mask1D(w)=1 ELSE no_changes=1

    mask2D=CONGRID(REFORM(mask1D, s(1), 1), s(1), s(2))
    w=WHERE(mask2D EQ 1)
    IF w(0) NE -1 THEN dat(w)=nan
  ENDREP UNTIL no_changes

  PRINT, FIX(TOTAL(mask1D)), ' rows to be replaced'
  IF TOTAL(mask1D) GT max_repl_lines THEN BEGIN
    PRINT, 'MORE THEN max_repl_lines: ', max_repl_lines, ' NO FILTERING !!!.'
    RETURN, filter1(data, -1, rx=4, ry=0) ; ONLY INF AND NAN ARE REMOVED
  ENDIF

  mask2D=CONGRID(REFORM(mask1D, s(1), 1), s(1), s(2))
  dat_new=filter1(dat, mask2D, rx=4, ry=0)
  RETURN, dat_new
END


FUNCTION Sino_FilterResc, s, sm=sm, thr=thr
  siz=SIZE(s)
  xsize=siz(1)
  nang=siz(2)

  w=WHERE(FINITE(s) EQ 0)
  s_finite=removenan(s)

  s_out=s_finite
  ssum=TOTAL(s_finite, 2)/nang

  w=WHERE(ssum GT thr)
  temp_ssum=ssum
  temp_ssum(*)=0
  IF w(0) NE -1 THEN temp_ssum(w)=1

  temp_ssmo=SMOOTH(temp_ssum, sm)
  ssmo=SMOOTH(ssum, sm)
  FOR i=0, xsize-1 DO BEGIN
    IF temp_ssmo(i) EQ 1 THEN s_out(i,*)=s_finite(i,*)/ssum(i)*ssmo(i)
  ENDFOR
  RETURN, s_out
END


FUNCTION Sino_FilterRescale, s, sm=sm
  siz=SIZE(s)
  xsize=siz(1)
  nang=siz(2)

  w=WHERE(FINITE(s) EQ 0)

  s_finite=s
  IF w(0) NE -1 THEN s_finite(w)=0  ; Set NaN to 0. - Will cause artifacts!

  s_out=s_finite
  ssum=TOTAL(s_finite, 2)/nang

  ssmo=SMOOTH(ssum, sm)
  FOR i=0, xsize-1 DO BEGIN
    IF ssum(i) GT (2.*.001) THEN s_out(i,*)=s_finite(i,*)/ssum(i)*ssmo(i)
  ENDFOR
  RETURN, s_out
END


FUNCTION Sino_FilterRescaleZ, y, par1=par1
  ;+
  ;par1 give extension over average region upwards and downwoards from slice
  ;-

  IF par1 LT 1 THEN BEGIN
    PRINT, 'par1 MUST BE >=1.'
    PRINT, 'RETALL'
    RETALL
  ENDIF

  s=getsis(y)
  siz=SIZE(s)
  xsize=siz(1)

  ysum_av=REPLICATE(0., xsize)
  ysum_n=0

  FOR i=1, par1 DO BEGIN
    s_temp=getsis(y+i)
    IF N_ELEMENTS(s_temp) NE 1 THEN BEGIN
      s_temp_finite=filter1(s_temp, -1, rx=4, ry=0)
      ysum_av=ysum_av + TOTAL(s_temp_finite, 2)
      ysum_n=ysum_n + 1
    ENDIF
    s_temp=getsis(y-i)
    IF N_ELEMENTS(s_temp) NE 1 THEN BEGIN
      s_temp_finite=filter1(s_temp, -1, rx=4, ry=0)
      ysum_av=ysum_av + TOTAL(s_temp_finite, 2)
      ysum_n=ysum_n + 1
    ENDIF
  ENDFOR

  s_finite=filter1(s, -1, rx=4, ry=0)
  ysum=TOTAL(s_finite, 2)

  ysum_av=ysum_av + ysum
  ysum_n=ysum_n + 1

  ysum_av=ysum_av/ysum_n

  s_out=s_finite

  FOR i=0, xsize-1 DO s_out(i,*)=s_finite(i,*)/ysum(i)*ysum_av(i)
  RETURN, s_out
END

FUNCTION Sino_ReadSortSis, y, ang_des=ang_des

  ;+
  ; READS THE sis FILES, WHICH CONTAIN THE ANGULAR POSITION AS THE FIRST
  ; COLUM (e.g. 134.25). THE ROWS ARE SORTED ACCORDING TO THEIR ANGULAR
  ; POSITIONS. POSITIONS WHICH OCCUR TWICE ARE REMOVED (ROW WITH LAST
  ; OCCURANCE IS USED). THE RETURNED SINOGRAM DOES NOT HAVE AN INDEX COLUMN.
  ; y IS THE SLICE NUMBER (HEIGHT)
  ;
  ;-

  ON_ERROR, 2

  max_diff_ang=.01

  recopfad=recon_abs_query('recopfad')
  scanname=recon_abs_query('scanname')
  filename=recopfad+'/sino/'+scanname+NUMSTR(y)+'.sis'
  tmp=read_dat(filename, /sil)   ;so is bigger then si by 1 in x-direction, first column contains angles


  s=SIZE(tmp)
  xsize=s(1) - 1
  ysize=s(2)

  ang=tmp(0,*)
  dat=tmp(1:*,*)

  s=SORT(ang)

  stepwinkel=FIX(recon_abs_query('stepwinkel'))
  ang_step=stepwinkel/100.

  scanmode=recon_abs_query('scanmode')
  CASE scanmode OF
    '180deg' : BEGIN
      maxdeg=180.
    END
    '360deg' : BEGIN
      maxdeg=360.
      ang=ang; + 180.
    END
    ELSE : MESSAGE, 'scanmode NOT SUPPORTED.'
  ENDCASE

  ;REMOVE MODULO OF 360 DEGREE
  ang=(ang + 360.) MOD 360.


  ;DEFINE THE ANGLES ang_des AT WHICH YOU WANT PROJECTION DATA
  IF N_ELEMENTS(ang_des) EQ 0 THEN BEGIN
    n_ang_des=ROUND(maxdeg/ang_step)
    ang_des=FINDGEN(n_ang_des)*ang_step
  ENDIF ELSE n_ang_des=N_ELEMENTS(ang_des)


  ;FIND THE CORRESPONDING DATA FOR THE DESIRED ANGLES ang_des
  ang_taken=REPLICATE(0., n_ang_des)
  dat_taken=REPLICATE(0., xsize, n_ang_des)

  FOR i_ang_des=0, n_ang_des - 1 DO BEGIN
    w=WHERE(ABS(ang - ang_des(i_ang_des)) LT max_diff_ang, count)
    IF count EQ 0 THEN MESSAGE, 'ERROR: No value found for angle: '+STRING(ang_des(i_ang_des))
    index=MAX(w) ;use last recorded value

    ang_taken(i_ang_des)=ang(index)
    dat_taken(*, i_ang_des)=dat(*, index)
  ENDFOR

  ;CHECK THE RESULT
  diff_ang=ang_des - ang_taken
  w=WHERE(ABS(diff_ang) GT 0.0001, count)
  ; stop
  IF count GE 1000 THEN PRINT, 'WARNING: Measured angles differ by maximum ', MAX(diff_ang(w)), count, ' points'

  RETURN, dat_taken
END

PRO Sino_FilterSino, filenum=filenum, check=check, thresh=thresh, ERASE=ERASE, filtname=filtname, par1=par1, par2=par2, scripted = scripted
  ;+
  ; Filter the sinograms, i.e. create sin-files (or si_ files for 360deg mode) from sis-files .
  ; Set /check to not redo existing filtered data, and filter all
  ; existing files in /sino.
  ; You can use different filters, which have to be specified as filtname='...'.
  ; The default filter is 'filter', which needs the parameter thresh.
  ;
  ; Filters:
  ;   'filter'
  ;   'rescale_filter'
  ;-

  IF N_ELEMENTS(silent) EQ 0 THEN silent=0
  IF N_ELEMENTS(scripted) EQ 0 THEN scripted=0
  IF N_ELEMENTS(check) EQ 0 THEN check=0
  IF N_ELEMENTS(ERASE) EQ 0 THEN ERASE=0
  IF N_ELEMENTS(filtname) EQ 0 THEN filtname='filter'
  
  ;   PRINT, 'THIS IS filtersino.pro VERSION 16.05.2003.'
  ;   PRINT, 'VERSION NAME BEFORE: filtersino6'
  
  IF filtname EQ 'filter' THEN BEGIN
    IF N_ELEMENTS(thresh) NE 1 THEN BEGIN
      PRINT, 'YOU HAVE TO SPECIFY thresh PARAMETER.'
      PRINT, 'RETALL.'
      RETALL
    ENDIF
    recon_abs_set, 'filtersino6_thresh', thresh
  ENDIF
  
  IF N_ELEMENTS(recopfad) EQ 0 THEN recopfad=seppath(get_reconlog())
  scanname=recon_abs_query('scanname')
  xmin=FIX(recon_abs_query('xmin'))
  xmax=FIX(recon_abs_query('xmax'))
  xanz=xmax-xmin+1
  ymin=FIX(recon_abs_query('ymin'))
  ymax=FIX(recon_abs_query('ymax'))
  yanz=ymax-ymin+1
  
  rawbin=FIX(recon_abs_query('rawbin'))
  ;yanz=yanz/rawbin
  
  if scripted eq 1 then begin
      recon_abs_set, 'xmin_sin', 0, /overwrite
      recon_abs_set, 'xmax_sin', xanz - 1, /overwrite
      recon_abs_set, 'ymin_sin', 0, /overwrite
      recon_abs_set, 'ymax_sin', yanz - 1, /overwrite
  endif

  if scripted eq 0 then begin
    recon_abs_set, 'xmin_sin', 0
    recon_abs_set, 'xmax_sin', xanz - 1
    recon_abs_set, 'ymin_sin', 0
    recon_abs_set, 'ymax_sin', yanz - 1
  endif
  ;READ THE scanmode
  scanmode=recon_abs_query('scanmode')
  CASE scanmode OF
    '180deg' : extout='sin' ; EXTENSION OF OUTPUT FILES
    '360deg' : extout='si_'
  ELSE : BEGIN
    PRINT, 'scanmode NOT SUPPORTED.'
    STOP
  END
ENDCASE

filtering=REPLICATE(FIX(0), ymax+1)
IF N_ELEMENTS(filenum) NE 0 THEN filtering(filenum)=1 ELSE filtering(0 : yanz-1)=1

IF check THEN BEGIN
  PRINT, 'Checking for existing files!'
  FOR i=0, ymax DO BEGIN
    IF (filtering(i) EQ 1) THEN BEGIN
      w_sis=file_search(recopfad+'/sino/'+scanname+numstr(i)+'.sis',co=w_sis_co)
      w_sin=file_search(recopfad+'/sino/'+scanname+numstr(i)+'.'+extout,co=w_sin_co)
      IF (w_sis_co EQ 0) OR (w_sin_co NE 0) THEN filtering(i)=0
    ENDIF
  ENDFOR
ENDIF
n_files=TOTAL(filtering)
PRINT, FIX(n_files), ' files found for processing!'
IF n_files EQ 0 THEN RETURN

processed_files=STRARR(n_files)
j=0
FOR i=0, N_ELEMENTS(filtering)-1 DO BEGIN
  IF filtering(i) EQ 1 THEN BEGIN
    sino_filename=recopfad+'/sino/'+scanname+numstr(i)+'.sis'
    sino_filt_filename=recopfad+'/sino/'+scanname+numstr(i)+'.'+extout
    
    dat=Sino_ReadSortSis(i)
    
    CASE filtname OF
      'filter' :  BEGIN
        IF thresh EQ 0 THEN BEGIN
          IF (j MOD 50) EQ 0 THEN BEGIN
            PRINT, 'filtersino.pro AT: ' + sino_filename + '  NO FILTERING. thresh=0'
          ENDIF
          sino_filt=filter1(dat, -1, rx=4, ry=4, ninf=ninf, nnan=nnan) ; ONLY INF AND NAN ARE REMOVED
          IF silent EQ 0 THEN IF (ninf NE 0) OR (nnan NE 0) THEN PRINT, sino_filename+ ' FOUND Inf: '+STRING(ninf)+ ' Nan: '+STRING(nnan)
        ENDIF ELSE sino_filt= Sino_FilterRaw(dat, thresh=thresh, max_repl_lines=200)
      END
      'rescale_filter' : BEGIN
        recon_abs_set, 'filter', filtname
        IF N_ELEMENTS(par1) EQ 0 THEN par1=20
        recon_abs_set, 'rescale_filter_par1', par1
        print, 'rescale_filter'
        sino_filt=Sino_FilterRescale(dat, sm=par1)
      END
      'resc_filter' : BEGIN
        recon_abs_set, 'filter', filtname
        IF N_ELEMENTS(par1) EQ 0 THEN par1=30
        IF N_ELEMENTS(par2) EQ 0 THEN par2=1.0
        recon_abs_set, 'resc_filter_par1', par1
        recon_abs_set, 'resc_filter_par2', par2
        print, 'rescale_filter'
        sino_filt=Sino_FilterResc(dat, sm=par1, thr=par2)
      END
      'rescale_filterz' :BEGIN
      recon_abs_set, 'filter', filtname
      IF N_ELEMENTS(par1) EQ 0 THEN par1=1
      recon_abs_set, 'rescale_filterz_par1', par1
      print, 'rescale_filterz'
      sino_filt=Sino_FilterRescaleZ(i, par1=par1)
    END
  ENDCASE
  
  write_dat, sino_filt, sino_filt_filename, /silent
  
  IF ERASE THEN BEGIN
    OPENR, lun, sino_filename, /GET_LUN, /delete
    CLOSE, lun
    FREE_LUN, lun
  ENDIF
  processed_files(j)=sino_filename
  j=j+1
ENDIF
ENDFOR
PRINT, 'Sino_filterSino procedure finished. ', j, ' files have been processed.'
END
