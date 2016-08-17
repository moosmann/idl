PRO Reco_FillRecoPipe, check=check, filenum=filenum, WAIT=WAIT $
    , priority=priority, silent=silent, rec_center=rec_center, slope = slope, sinobin = sinobin
  ;+
  ;  Set /check to not redo existing .sli or .int files and only
  ;  work on existing .sin-files
  ;  Setting wait keyword will wait for all reconstructions to finish
  ;  Set priority to set the position in the reconstruction-queue
  ;-
    
  IF N_ELEMENTS(check) EQ 0 THEN check=0
  IF N_ELEMENTS(WAIT) EQ 0 THEN WAIT=0
  IF N_ELEMENTS(priority) EQ 0 THEN priority=0
  IF N_ELEMENTS(silent) EQ 0 THEN silent=0
  CASE silent OF
    0 : message_after=50
    1 : message_after=1999 ;
    2 : message_after=1
  ENDCASE
  IF N_ELEMENTS(ERASE) EQ 0 THEN ERASE=0
  
  IF N_ELEMENTS(recopfad) EQ 0 THEN recopfad=seppath(get_reconlog())

  IF N_ELEMENTS(slope) EQ 0 THEN BEGIN
    slope = 0
  ENDIF ELSE BEGIN
    slope = float(slope)
    recon_abs_set, 'rec_center_slope', slope, /overwrite
  ENDELSE

  IF N_ELEMENTS(rec_center) EQ 0 THEN BEGIN
    zen=FLOAT(recon_abs_query('reczentrum')) 
    ENDIF ELSE BEGIN
    zen=rec_center
    IF slope EQ 0 THEN recon_abs_set, 'reczentrum', rec_center, /overwrite $
                  ELSE recon_abs_set, 'rec_center_0', rec_center, /overwrite
  ENDELSE

  IF N_ELEMENTS(sinobin) EQ 0 THEN begin
      sinobin = 1
      endif ELSE begin
      recon_abs_set, 'sinobin', sinobin, /overwrite
      recon_abs_set, 'y_bi_min', recon_abs_query('ymin') / sinobin, /overwrite
      recon_abs_set, 'y_bi_max', recon_abs_query('ymax') / sinobin, /overwrite
  endelse ;N_ELEMENTS(sinobin)
  
  path=recopfad+'/reco'
  
  PRINT, 'THIS IS local hostname: '+get_hostname()
  PRINT, 'recopfad: ' +recopfad
  recopfad_host=recon_abs_query('recopfad_host')
  recopfad_host_os=recon_abs_query('recopfad_host_os')
  
  IF (silent NE 1) THEN PRINT, 'CREATING DIRECTORY: '+path
  FILE_MKDIR, path
  
  scanname=recon_abs_query('scanname')
  sinobin=FIX(recon_abs_query('sinobin'))
  
  IF sinobin EQ 1 THEN BEGIN
    ymin=FIX(recon_abs_query('ymin'))
    ymax=FIX(recon_abs_query('ymax'))
    rawbin=FIX(recon_abs_query('rawbin'))
    ymax=(ymax-ymin+1)-1
    ymin=0
  ENDIF ELSE BEGIN
    ymin=FIX(recon_abs_query('y_bi_min'))
    ymax=FIX(recon_abs_query('y_bi_max'))
  ENDELSE
  
  IF N_ELEMENTS(filenum) NE 0 THEN BEGIN
    sli_erz=REPLICATE(FIX(0), MAX(filenum)+1)
    sli_erz(filenum)=1
  ENDIF ELSE BEGIN
    sli_erz=REPLICATE(FIX(0), ymax+1)
    sli_erz(ymin : ymax)=1
  ENDELSE
  IF check THEN BEGIN
    PRINT, 'Checking for existing files!'
    FOR i=0, N_ELEMENTS(sli_erz)-1 DO BEGIN
      IF (sli_erz(i) EQ 1) THEN BEGIN
        w_sin=file_search(recopfad+'/sino/'+scanname+numstr(i)+'.sin',co=w_sin_co)
        w_sli=file_search(recopfad+'/reco/'+scanname+numstr(i)+'.sli',co=w_sli_co)
        w_tif=file_search(recopfad+'/tiff/'+scanname+numstr(i)+'.tif',co=w_tif_co)
        w_int=file_search(recopfad+'/reco/'+scanname+numstr(i)+'.int',co=w_int_co)
        IF (w_sin_co EQ 0) OR (w_sli_co NE 0) OR (w_tif_co NE 0)  OR (w_int_co NE 0)THEN sli_erz(i)=0
      ENDIF
    ENDFOR
  ENDIF
  n_files=FIX(TOTAL(sli_erz))
  IF n_files EQ 0 THEN BEGIN
    PRINT, 'No files found for reconstruction in: '+recopfad+'/sino/'+scanname
    RETURN
  ENDIF ELSE IF (silent NE 1) THEN PRINT, n_files, ' found for reconstruction.'
  
  files=STRARR(n_files)
  j=0
  FOR i=0, N_ELEMENTS(sli_erz)-1 DO BEGIN
    IF sli_erz(i) EQ 1 THEN BEGIN
    
      sli_filename=trimpath(recopfad+'/reco/'+scanname+numstr(i)+'.sli', os=recopfad_host_os)
      sin_filename=trimpath(recopfad+'/sino/'+scanname+numstr(i)+'.sin', os=recopfad_host_os)
      
      IF (silent NE 1) AND ((j MOD message_after) EQ 0) THEN silent_now=0 ELSE silent_now=1
      ;set slope of reco center
      if slope NE 0 THEN zen = rec_center + i * slope
      one_slice, recopfad_host, sin_filename, sli_filename, FLOAT(zen), priority, silent=silent_now, /keep_pipe
      if ((i mod 50) eq 0) then print, 'Slice ', i, ' / rec_center = ', zen 

      files(j)=recopfad+'/reco/'+scanname+numstr(i)+'.sli'
      j=j+1
    ENDIF
  ENDFOR
  rec_pipe_close
  IF (silent NE 1) THEN PRINT,'reconlogerz.pro END - ALL SLICES INSERTED.'
  
  IF WAIT THEN waitforfiles, recopfad+'/reco/*', files
  
END


PRO Reco_FindRotCenter, startcenter=startcenter, noabort=noabort, ignorelimits = ignorelimits
    if n_elements(noabort) eq 0 then noabort = 0
    if N_ELEMENTS(startcenter) EQ 0 then startcenter = 0
    if n_elements(ignorelimits) eq 0 then  _ignorelimits = 0    else _ignorelimits = 1
   
    ymin=FIX(recon_abs_query('ymin'))
    ymax=FIX(recon_abs_query('ymax'))
    sinobin=FIX(recon_abs_query('sinobin'))
    rawbin=FIX(recon_abs_query('rawbin'))
    yanz=((ymax-ymin)+1)/sinobin/rawbin
    _centernotfound = 0
    
    FOR j=1,1 DO BEGIN
        IF (j EQ 0) THEN y=100
        IF (j EQ 1) THEN y=yanz/2
        IF (j EQ 2) THEN y=900
        y=(fix(recon_abs_query('ymin'))+fix(recon_abs_query('ymax')))/2
        if recon_abs_query('scanmode') eq '360deg' then y=(fix(recon_abs_query('ymax'))-fix(recon_abs_query('ymin')))/2/sinobin
        PRINT, 'Slice in the middle is y: ', y
        s=getsinmean(y-10, 21)
        IF startcenter EQ 0 THEN BEGIN
            rp=cor_iter(s, dtr_succ=dtr_succ, dtr_start=64, ignorelimits = _ignorelimits)
        ENDIF ELSE BEGIN
            rp=cor_iter(s, dtr_succ=dtr_succ, dtr_start=64, tr0_start=startcenter, ignorelimits = _ignorelimits)
        ENDELSE
      
        ;Which resolution was reached? Abort, when greater than one pixel precission
        if noabort eq 0 then begin 
            IF (dtr_succ EQ -1) THEN MESSAGE, 'ERROR: No result by cor_iter'
            IF (dtr_succ GT 1.0) THEN MESSAGE, 'ABORT: cor_iter returned precission above 1.0 pixel.'
        endif else begin
            if (dtr_succ) lt 0 or (dtr_succ gt 0.3) then _centernotfound = 1 
        endelse ;noabort eq 0
        ;Round to .05 resolution and add 1. for FORTRAN notation in reclbl
        rp_use=ROUND(rp*20)/20. + 1.
        PRINT, 'Best rp for reconstruction (FORTRAN): ', rp_use
    ENDFOR
    recon_abs_set, 'reczentrum', rp_use
    if _centernotfound eq 1 then recon_abs_set, 'reczentrum', -1, /overwrite
END ;pro Reco_FindRotCenter




PRO Reco_RecoWithVarRecCenter, rzmin, rzmax, rzstep, WAIT=hwait, silent=silent, $
    SliceNo=SliceNo, check=check, buter_order=buter_order, $
    buter_freq=buter_freq, qual=qual, files=files
;+
; Call: reconlogtesterz, rzmin, rzmax, rzstep, SliceNo=[512]
; Parameter 'files' returns a list of files, that have been requested by the call.
;-

   ON_ERROR, 1 ; Return to main on Error

   IF N_ELEMENTS(qual) EQ 0 THEN qual=0

   IF N_ELEMENTS(SliceNo) EQ 0 THEN BEGIN
       PRINT, 'reconlogtesterz: YOU MUST SPECIFY THE PARAMETER SliceNo.'
       RETALL
   ENDIF
   IF N_ELEMENTS(hwait) EQ 0 THEN hwait=1

   IF N_ELEMENTS(rzstep) EQ 0 THEN rzstep=1.
   IF N_ELEMENTS(rzmin) EQ 0 THEN BEGIN
     temp=getsin(SliceNo)
     IF N_ELEMENTS(temp) EQ 1 THEN BEGIN
       PRINT, 'file does not exist. RETALL.'
       RETALL
     ENDIF
     dp=sino_drehpunkt(temp)
     rzmin=ROUND(dp)-7
   ENDIF
   IF N_ELEMENTS(rzmax) EQ 0 THEN rzmax=ROUND(dp)+7

   IF N_ELEMENTS(recopfad) EQ 0 THEN recopfad=seppath(get_reconlog())

   scanname=recon_abs_query('scanname')
   PRINT, 'THIS IS local hostname: '+get_hostname()
   recopfad_host=recon_abs_query('recopfad_host')
   PRINT, 'recopfad: ' +recopfad
   recopfad_host_os=recon_abs_query('recopfad_host_os')

   path=recopfad+'/reco'

   PRINT, 'CREATING DIRECTORY: '+path
   FILE_MKDIR, path

   ncenter=0
   FOR j=rzmin*100l, rzmax*100l, rzstep*100l DO ncenter=ncenter+1
   nz=N_ELEMENTS(SliceNo)
   n_files=ncenter*nz

   files=REPLICATE('', n_files)
   files_new=REPLICATE(0, n_files) ; set to 1 if file has to be created
   file_count=0

   FOR i=0, nz-1 DO BEGIN
      fileno=SliceNo(i)
      FOR j=rzmin*100l, rzmax*100l, rzstep*100l DO BEGIN
        sin_filename=trimpath(recopfad+'/sino/'+scanname+numstr(fileno)+'.sin', os=recopfad_host_os)
        sli_filename=trimpath(recopfad+'/reco/'+scanname+numstr(fileno)+'.'+numstr(j), os=recopfad_host_os)
        IF file_exists(sli_filename) EQ 0 THEN BEGIN
          files_new(file_count)=1
          priority=-2
          rec_center=j/100.
          one_slice, recopfad_host, sin_filename, sli_filename, rec_center, priority, /keep_pipe, buter_order=buter_order, buter_freq=buter_freq
        ENDIF ELSE PRINT, sli_filename+' already exists.'
        files(file_count)=sli_filename
        file_count=file_count+1
      ENDFOR
   ENDFOR

   rec_pipe_close
   IF N_ELEMENTS(hwait) EQ 0 THEN hwait=0
   w=WHERE(files_new EQ 1)
   IF w(0) EQ -1 THEN hwait=0  ; DONT wait IF NO FILES ARE THERE TO WAIT FOR.
   IF hwait THEN waitforfiles, trimpath(recopfad+'/reco/*'), trimpath(files(w)), silent=silent

   IF (qual EQ 1) THEN BEGIN
     qual_par=REPLICATE(0., N_ELEMENTS(files))
     center=REPLICATE(0., N_ELEMENTS(files))
     FOR i=0, N_ELEMENTS(files)-1 DO BEGIN
       qual_par(i)=cenq(read_dat(files(i)))
       temp=STRSPLIT(files(i), '.', /extract)
       center(i)=FLOAT(temp(1))/100.
     ENDFOR
     PLOT, center, qual_par, psym=-2, /ynozero
     WSHOW
     IF global_minimum(qual_par, minpos=minpos) THEN BEGIN
       PRINT, 'Globales Minimum: ', minpos, center(minpos)
     ENDIF
   ENDIF
END



pro Reco_TestShow, slice = slice, logfile = logfile, keeplog = keeplog,$
                   scalemin = scalemin, scalemax = scalemax, bin = bin, $
                   crop = crop, qual = qual
    COMMON recon_abs_name, reconlogname
    
    recopfad=seppath(get_reconlog())
    scanname=recon_abs_query('scanname')
    
    if n_elements(slice)    eq 0    then _slice = -1         else _slice = slice
    if n_elements(scalemin) eq 0    then scmin = -0.001  else scmin = scalemin
    if n_elements(scalemax) eq 0    then scmax = 0.004   else scmax = scalemax
    if n_elements(keeplog)  eq 0 $
      and n_elements(logfile) eq 0  then set_reconlog
    if n_elements(slice)  ne 0    then n=FINDFILE(recopfad+'/reco/'+scanname+NUMSTR(slice(0))+'*',co=co) $
                                    else n=FINDFILE(recopfad+'/reco/'+scanname+'*',co=co)
    if n_elements(bin)      eq 0    then bin = 1 else bin = bin
    if n_elements(qual)     eq 0    then qual = 0 else qual = qual
    if n_elements(crop)     ne 0    then _cropping = 1 else _cropping = 0  
    if n_elements(logfile) ne 0     then begin
      recon_abs_set, 'recopfad', logfile, /overwrite
      reconlogname = logfile
    endif

    
    ;if _slice eq -1 then recontestshow, _scalemin, _scalemax    $
    ;                else recontestshow, _scalemin, _scalemax, filenum = _slice
    
    neu=1
  
    IF co EQ 0 THEN BEGIN
      PRINT, 'recontestshow: NO FILES FOUND'
      RETURN
    ENDIF
  
    center=REPLICATE(0., co)
    qual_par1=REPLICATE(0., co)
    qual_par2=REPLICATE(0., co)
    winnum_img=!D.WINDOW
    i=0
    ende=0
    first=1
  
    REPEAT BEGIN
        s=read_dat(n(i))
        print, size(s, /dimensions)
        if _cropping eq 1 then s = s[crop[0]:crop[1], crop[2]:crop[3]]
        temp=STRSPLIT(n(i), '.', /extract)
        IF temp(1) NE 'sli' THEN center(i)=FLOAT(temp(1))/100. ELSE center(i)=-1
        IF (first EQ 1) THEN BEGIN
            IF (neu EQ 1) OR (!D.WINDOW EQ -1) THEN $ 
                winn, BYTSCL(bin2d(s, bin),scmin,scmax), winnum=winnum_img, index=index_img
        ENDIF ELSE BEGIN
            WSET, winnum_img
            WSHOW
        ENDELSE
        TV, BYTSCL(bin2d(s, bin),scmin,scmax)
    
        c=STRUPCASE(GET_KBRD(1))
        IF c EQ 'M' THEN BEGIN
            IF i GT 0 THEN i=i-1
        ENDIF ELSE BEGIN
            IF (c EQ 'S') OR (c EQ 'Q') THEN ende=1
            IF (c EQ ' ') and i lt co-1 THEN i=i+1
            IF (c EQ 'P') and (i lt co-5) THEN i=i+5  
        ENDELSE

        first=0
    ENDREP UNTIL ende EQ 1
  
    WIDGET_CONTROL, index_img(1), /destroy
  
    IF qual EQ 1 THEN BEGIN
      w=WHERE(center NE 0.)
      center=center(w)
      qual_par1=qual_par1(w)
      minpos=global_minimum(qual_par1, minpos=minpos)
      IF minpos NE -1 THEN PRINT, 'Globales Minimum: ', minpos, center(minpos)
    ENDIF
    
end ;Reco_TestShow