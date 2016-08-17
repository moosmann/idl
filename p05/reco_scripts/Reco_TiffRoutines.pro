;=======================================
PRO Tiff_Make, ERASE=era, filenum=filenum, check=check, MIN=MIN, MAX=MAX, recopfad=recopfad

;+
; Create .tif files from the .sli file.
; Set /check to not redo existing data, and process all existing files in /reco
; Existing .tif files in in the tiff directory will be replaced!
;-
   IF NOT KEYWORD_SET(check) THEN check=0

;   recopfad=recon_abs_query('recopfad')
  IF N_ELEMENTS(recopfad) EQ 0 THEN recopfad=seppath(get_reconlog())
   scanname=recon_abs_query('scanname')
   ymin=FIX(recon_abs_query('ymin'))
   ymax=FIX(recon_abs_query('ymax'))
   sinobin=FIX(recon_abs_query('sinobin'))
   rawbin=recon_abs_query('rawbin')
   yanz=(ymax-ymin+1);/sinobin/rawbin

   IF N_ELEMENTS(MIN) EQ 1 THEN recon_abs_set, 'tiffmin', MIN
   tiffmin=FLOAT(recon_abs_query('tiffmin'))
   IF N_ELEMENTS(MAX) EQ 1 THEN recon_abs_set, 'tiffmax', MAX
   tiffmax=FLOAT(recon_abs_query('tiffmax'))

   path=recopfad+'/tiff'
   PRINT, 'CREATING DIRECTORY: '+path
   FILE_MKDIR, path

   IF KEYWORD_SET(filenum) THEN BEGIN
      tiffing=REPLICATE(FIX(0), MAX(filenum)+1)
      tiffing(filenum)=1
   ENDIF ELSE BEGIN
      tiffing=REPLICATE(FIX(0), ymax+1)
      tiffing(0:yanz-1)=1
   ENDELSE

   IF check THEN BEGIN
      PRINT, 'Checking for existing files!'

      files_sli=FINDFILE(recopfad+'/reco/'+scanname+'*.sli')
      files_tif=FINDFILE(recopfad+'/tiff/'+scanname+'*.tif')
      FOR i=0, N_ELEMENTS(tiffing)-1 DO BEGIN
         IF (tiffing(i) EQ 1) THEN BEGIN
            w_sli=file_search(recopfad+'/reco/'+scanname+numstr(i)+'.sli',co=w_sli_co)
            w_tif=file_search(recopfad+'/tiff/'+scanname+numstr(i)+'.tif',co=w_tif_co)
            IF (w_sli_co EQ 0) OR (w_tif_co NE 0) THEN tiffing(i)=0
         ENDIF
      ENDFOR
   ENDIF

   n_files=TOTAL(tiffing)
   IF n_files EQ 0 THEN BEGIN
      PRINT, 'No files found for processing!'
      RETURN
   ENDIF


   PRINT, 'Start of sli to tif conversion!'
   j=0

   FOR i=0, N_ELEMENTS(tiffing)-1 DO BEGIN

      IF tiffing(i) EQ 1 THEN BEGIN

         reco_filename=recopfad+'/reco/'+scanname+numstr(i)+'.sli'
         tiff_filename=recopfad+'/tiff/'+scanname+numstr(i)+'.tif'

;NOTICE: tiff_write and tiff_read are no longer up to date. Use: read_tiff and write_tiff.
         tiff_write, tiff_filename, BYTSCL(read_dat(reco_filename, /silent, /nocheck), tiffmin, tiffmax)

         IF KEYWORD_SET(era) THEN BEGIN
            OPENR, lun, reco_filename, /GET_LUN, /delete
            CLOSE, lun
            FREE_LUN, lun
         ENDIF

         IF (j MOD 50 EQ 0) OR (j EQ TOTAL(tiffing)-1) THEN BEGIN
            PRINT, 'maketiff> '+reco_filename
         ENDIF

         j=j+1
      ENDIF
   ENDFOR
   PRINT, 'maketiff> finished. ', j , ' files have been processed.'
END

;============================================
PRO rescale_tiff, newtiffmin, newtiffmax, filenum=filenum, check=check

   IF NOT KEYWORD_SET(check) THEN check=0

   recopfad=recon_abs_query('recopfad')
   scanname=recon_abs_query('scanname')
   ymin=FIX(recon_abs_query('ymin'))
   ymax=FIX(recon_abs_query('ymax'))
   rawbin=recon_abs_query('rawbin')
   yanz=(ymax-ymin+1)/sinobin/rawbin

   tiffmin=FLOAT(recon_abs_query('tiffmin'))
   tiffmax=FLOAT(recon_abs_query('tiffmax'))

   PRINT, 'NEW FILES WILL BE STORED IN ', recopfad+'/tiffnew'
   SPAWN, 'mkdir '+recopfad+'/tiffnew'


   IF KEYWORD_SET(filenum) THEN BEGIN
      tiffing=REPLICATE(FIX(0), MAX(filenum)+1)
      tiffing(filenum)=1
   ENDIF ELSE BEGIN
      tiffing=REPLICATE(FIX(0), ymax+1)
      tiffing(0:yanz-1)=1
   ENDELSE

   IF check THEN BEGIN
      PRINT, 'Checking for existing files!'

      files_tif=FINDFILE(recopfad+'/tiff/'+scanname+'*.tif')
      files_newtif=FINDFILE(recopfad+'/tiffnew/'+scanname+'*.tif')
      FOR i=0, N_ELEMENTS(tiffing)-1 DO BEGIN
         IF (tiffing(i) EQ 1) THEN BEGIN
            w_tif=WHERE(files_sli EQ recopfad+'/tiff/'+scanname+numstr(i)+'.tif')
            w_newtif=WHERE(files_tif EQ recopfad+'/tiffnew/'+scanname+numstr(i)+'.tif')
            IF (w_tif EQ -1) OR (w_newtif NE -1) THEN tiffing(i)=0
         ENDIF
      ENDFOR
   ENDIF

   n_files=TOTAL(tiffing)
   IF n_files EQ 0 THEN BEGIN
      PRINT, 'No files found for processing!'
      RETURN
   ENDIF


   PRINT, 'Start of sli to tif conversion!'
   j=0

   FOR i=0, N_ELEMENTS(tiffing)-1 DO BEGIN

      IF tiffing(i) EQ 1 THEN BEGIN

         tiff_filename=recopfad+'/tiff/'+scanname+numstr(i)+'.tif'
         newtiff_filename=recopfad+'/tiffnew/'+scanname+numstr(i)+'.tif'

         tiff_write, newtiff_filename, BYTSCL(FLOAT(tiff_read(tiff_filename))*(tiffmax-tiffmin)/255.+tiffmin, newtiffmin, newtiffmax)

         IF KEYWORD_SET(era) THEN BEGIN
            OPENR, lun, tiff_filename, /GET_LUN, /delete
            CLOSE, lun
            FREE_LUN, lun
         ENDIF

         IF (j MOD 50 EQ 0) OR (j EQ TOTAL(tiffing)-1) THEN BEGIN
            PRINT, tiff_filename
         ENDIF

         j=j+1
      ENDIF
   ENDFOR
   PRINT, 'recscale_tiff.pro  has finished. ', j , ' files have been processed.'

   PRINT, 'YOU CAN NOW CHANGE THE VALUES IN reconlog.txt'
   recon_abs_set, 'tiffmin', newtiffmin
   recon_abs_set, 'tiffmax', newtiffmax

END



;      w=where(si lt -32768)&if w(0) ne -1 then si(w)=-32768
;      w=where(si gt 32767)&if w(0) ne -1 then si(w)=32767
;      si=fix(si)
;      write_dat, si, recopfad+'/reco/'+STRMID(n(i), 0, STRLEN(n(i))-4)+'.int'
