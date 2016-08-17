;================================
FUNCTION get_recserv_master, dummy
;+
; Returns the hostname of the computer running recserv MASTER.
;-
   COMMON recserv_master, recserv_master
;   IF N_ELEMENTS(recserv_master) EQ 0 THEN BEGIN
;      recserv_master=''
;      READ, recserv_master, prompt='recserv_master_host'
;      recserv_master=STRTRIM(recserv_master, 2)
;   ENDIF
   IF N_ELEMENTS(recserv_master) EQ 1 THEN RETURN, recserv_master $
                                      ELSE RETURN, ''
END

;===========================
PRO set_recserv_master, host
;+
; E.g.: set_recserv_master, 'hasgkssxtm00a.desy.de'
;-
   COMMON recserv_master, recserv_master

   IF N_ELEMENTS(host) EQ 0 THEN BEGIN
      PRINT, 'EXAMPLE OF USE:  set_recserv_master, '+ STRING(39b)+'hasxtm8.desy.de'+ STRING(39b)
      RETURN
   ENDIF

   IF N_ELEMENTS(recserv_master) NE 0 THEN PRINT, 'old recserv_master: ' +recserv_master

   recserv_master=host

   PRINT, 'recserv_master: ' +recserv_master

   ;SPAWN, 'PING '+recserv_master

END


;===================================
FUNCTION rec_pipe_open, rec_computer
   COMMON one_slice_common, ri

   IF (N_ELEMENTS(ri) EQ 0) THEN ri=OBJ_NEW()
   IF (ri EQ OBJ_NEW()) THEN BEGIN
      PRINT, 'opening pipe to: ' +rec_computer
      ri=OBJ_NEW('rec_insert', rec_computer)
      WAIT, .1
   ENDIF
   IF ri EQ OBJ_NEW() THEN BEGIN
      PRINT, 'ERROR OPENING SOCKET'
      STOP   ;leeres socket - fehler aufgetreten.
   ENDIF

   RETURN, ri
END

;=========================
FUNCTION rec_pipe_status
   COMMON one_slice_common, ri
   IF (N_ELEMENTS(ri) EQ 0) OR (ri EQ OBJ_NEW()) THEN pipe_open=0 ELSE pipe_open=1
   RETURN, pipe_open
END

;=================
PRO rec_pipe_close
   COMMON one_slice_common, ri

   IF N_ELEMENTS(ri) EQ 0 THEN BEGIN
     PRINT, 'rec_pipe_close: Not a valid pipe. RETURNING.'
     RETURN
   ENDIF

   PRINT, 'CLOSING PIPE.'
   OBJ_DESTROY, ri
   ri=OBJ_NEW()

END


;==========================================================================================
PRO one_slice, dat_computer, sin_filename, sli_filename, rec_center, priority, buter_order=buter_order, buter_freq=buter_freq, silent=silent, keep_pipe=keep_pipe $
    , recstartangle=recstartangle
;+
;Procedure enters a single reconstruction into the rec server.
;-
sinerr_filename=sin_filename+'err'
slierr_filename=sli_filename+'err'
startangle=float(recon_abs_query('recostartangle'))
recontype=fix(recon_abs_query('reconstructiontype'))

check_here=1 ; check if the file exists on this computer!!! Could give problems, with sin files on remote systems

   IF N_ELEMENTS(silent) EQ 0 THEN silent=0
   IF N_ELEMENTS(keep_pipe) EQ 0 THEN keep_pipe=0

   rec_computer=get_recserv_master()
   IF rec_computer EQ '' THEN BEGIN
      PRINT, 'NO recserv_master DEFINED. USE THE COMMAND set_recserv_master, %%%%%% BEFORE YOU CONTINUE
      STOP
   ENDIF
;   PRINT, 'Using recserv_MASTER_host: '+rec_computer

   IF N_ELEMENTS(rec_center) EQ 0 THEN BEGIN
      PRINT, 'NO rec_center DEFINED IN one_slice.'
      PRINT, 'RETALL.'
      RETALL
   ENDIF

   IF N_ELEMENTS(priority) EQ 0 THEN priority=0
   IF N_ELEMENTS(buter_order) EQ 0 THEN buter_order=10.
   IF N_ELEMENTS(buter_freq)  EQ 0 THEN buter_freq =.5

   IF check_here THEN BEGIN
     IF file_exists(sin_filename) EQ 0 THEN BEGIN
       PRINT, sin_filename +' NOT FOUND IN one_slice.pro'
       PRINT, 'RETALL'
       RETALL
     ENDIF
     IF file_exists(sli_filename) THEN BEGIN
       beep
       answer=DIALOG_MESSAGE('one_slice.pro: Are you silly?  FILE EXISTS: '+sli_filename+'. Do you want to overwrite the existing file?', /question, /cancel, /default_no)
       IF answer EQ 'Cancel' THEN RETALL
       IF answer EQ 'No' THEN RETURN
     ENDIF
   ENDIF

   IF (silent NE 1) THEN PRINT, 'one_slice, '+rec_computer+', '+sin_filename+', '+sli_filename+', '+STRING(rec_center)+', '+STRING(priority)


   ;OPEN PIPE FOR RECONSTRUCTION IF NOT OPEN
;stop
   ri=rec_pipe_open(rec_computer)

   ;ENTER SLICE INTO RECONSTRUCTION
   ri->sino, rec_computer, dat_computer, sin_filename, sinerr_filename, FLOAT(rec_center), FIX(startangle), FIX(recontype), buter_order, buter_freq, priority
   ri->slice, dat_computer, sli_filename, slierr_filename, 'F',0.,0.,0,0,0,0
   ri->go

   ;CLOSE PIPE
   IF (keep_pipe EQ 0) THEN rec_pipe_close

   ;This is a test - can this avoid the segmentation fault at the recserv /MASTER ?
   WAIT, .1
END



;****************************************************************
PRO reconlogtesterz, rzmin, rzmax, rzstep, WAIT=hwait, silent=silent, $
    filenum=filenum, check=check, buter_order=buter_order, $
    buter_freq=buter_freq, qual=qual, files=files
;+
; Call: reconlogtesterz, rzmin, rzmax, rzstep, filenum=[512]
; Parameter 'files' returns a list of files, that have been requested by the call.
;-

   ON_ERROR, 1 ; Return to main on Error

   IF N_ELEMENTS(qual) EQ 0 THEN qual=0

   IF N_ELEMENTS(filenum) EQ 0 THEN BEGIN
       PRINT, 'reconlogtesterz: YOU MUST SPECIFY THE PARAMETER filenum.'
       RETALL
   ENDIF
   IF N_ELEMENTS(hwait) EQ 0 THEN hwait=1

   IF N_ELEMENTS(rzstep) EQ 0 THEN rzstep=1.
   IF N_ELEMENTS(rzmin) EQ 0 THEN BEGIN
     temp=getsin(filenum)
     IF N_ELEMENTS(temp) EQ 1 THEN BEGIN
       PRINT, 'file does not exist. RETALL.'
       RETALL
     ENDIF
     dp=sino_drehpunkt(temp)
     rzmin=ROUND(dp)-7
   ENDIF
   IF N_ELEMENTS(rzmax) EQ 0 THEN rzmax=ROUND(dp)+7

;   recopfad=recon_abs_query('recopfad')
   IF N_ELEMENTS(recopfad) EQ 0 THEN recopfad=seppath(get_reconlog())

   scanname=recon_abs_query('scanname')
   PRINT, 'THIS IS local hostname: '+get_hostname()
   recopfad_host=recon_abs_query('recopfad_host')
;   PRINT, 'TELL ME recopfad_host OPERATING SYSTEM FAMILY (Windows, UNIX)' ; necessary for trimpath
   PRINT, 'recopfad: ' +recopfad
   recopfad_host_os=recon_abs_query('recopfad_host_os')


   path=recopfad+'/reco'

   PRINT, 'CREATING DIRECTORY: '+path
   FILE_MKDIR, path

   ncenter=0
   FOR j=rzmin*100l, rzmax*100l, rzstep*100l DO ncenter=ncenter+1

   nz=N_ELEMENTS(filenum)
   n_files=ncenter*nz

   files=REPLICATE('', n_files)
   files_new=REPLICATE(0, n_files) ; set to 1 if file has to be created
   file_count=0

   FOR i=0, nz-1 DO BEGIN
      fileno=filenum(i)
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

   ;
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


;********************************************
FUNCTION recon_now, SIN, center, filename=filename, buter_order=buter_order, buter_freq=buter_freq
;+
;not yet adapted

; WORKS WITH DATA IN THE COMPUTER MEMORY, WHICH WILL BE WRITTEN
; TO A TEMPORARY FILE FOR RECONSTRUCTION
;-

   IF N_ELEMENTS(center) EQ 0 THEN center=sino_drehpunkt(SIN)
   PRINT, 'rec center (FORTRAN): ', center

   IF N_ELEMENTS(filename) EQ 0 THEN filename='reco'

   path='c:\temp'
;path='.'
   sin_filename=trimpath(path+'\'+filename+'_sin.tmp')
   sli_filename=trimpath(path+'\'+filename+'_sli.tmp')
   write_dat, FLOAT(SIN), sin_filename
   WAIT, 1.

   dat_computer='hasxtm8.desy.de'
   priority=-2

   one_slice, dat_computer, sin_filename, sli_filename, center, priority, buter_order=buter_order, buter_freq=buter_freq

   start_time=SYSTIME(0, /seconds)
;   PRINT, 'SENT RECONSTRUCTION TO /hasyw04. (', LONG(n_ang)*nx*ny/1000000,' million operations.)
   waitforfiles, path+'\*', sli_filename
   end_time=SYSTIME(0, /seconds)
   PRINT, 'DONE IN ', FIX(end_time-start_time), ' SECONDS.'

   WAIT, 3.
   dat=read_dat(sli_filename)

   IF file_exists(sin_filename) THEN FILE_DELETE, sin_filename
   IF file_exists(sli_filename) THEN FILE_DELETE, sli_filename

   RETURN, dat
END



;****************************************************************
PRO reconlogerz, check=check, filenum=filenum, WAIT=WAIT $
               , priority=priority, silent=silent, rec_center=rec_center
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

   ;recopfad=recon_abs_query('recopfad')
   IF N_ELEMENTS(recopfad) EQ 0 THEN recopfad=seppath(get_reconlog())

   path=recopfad+'/reco'

   PRINT, 'THIS IS local hostname: '+get_hostname()
   PRINT, 'recopfad: ' +recopfad
   recopfad_host=recon_abs_query('recopfad_host')
;   PRINT, 'TELL ME recopfad_host OPERATING SYSTEM FAMILY (Windows, UNIX)' ; necessary for trimpath
   recopfad_host_os=recon_abs_query('recopfad_host_os')

   IF (silent NE 1) THEN PRINT, 'CREATING DIRECTORY: '+path
   FILE_MKDIR, path

   scanname=recon_abs_query('scanname')
   IF N_ELEMENTS(rec_center) EQ 0 THEN zen=FLOAT(recon_abs_query('reczentrum')) ELSE zen=rec_center
   sinobin=FIX(recon_abs_query('sinobin'))

;   PRINT, 'ymin / ymax               : ', FIX(recon_abs_query('ymin', /noinput)), ' / ',FIX(recon_abs_query('ymax', /noinput))
;   PRINT, 'sinobin: ', sinobin
;   PRINT, 'use --> si_ymin / si_ymax : ', FIX(0), ' / ',  (FIX(recon_abs_query('ymax', /noinput))-FIX(recon_abs_query('ymin', /noinput)))/sinobin
;
;   ymin=fix(recon_abs_query('si_ymin'))
;   ymax=fix(recon_abs_query('si_ymax'))
;   yanz=ymax-ymin+1
;
;   xmin=fix(recon_abs_query('xmin'))
;   xmax=fix(recon_abs_query('xmax'))
;   xanz=(xmax-xmin+1)/sinobin

   IF sinobin EQ 1 THEN BEGIN
      ymin=FIX(recon_abs_query('ymin'))
      ymax=FIX(recon_abs_query('ymax'))
      rawbin=FIX(recon_abs_query('rawbin'))
      ymax=(ymax-ymin+1)/rawbin-1
      ymin=0

;      xmin=FIX(recon_abs_query('xmin')) ;Zeile entfernt TD 21.12.05
;      xmax=FIX(recon_abs_query('xmax')) ;Zeile entfernt TD 21.12.05
   ENDIF ELSE BEGIN
      ymin=FIX(recon_abs_query('y_bi_min'))
      ymax=FIX(recon_abs_query('y_bi_max'))
;      xmin=FIX(recon_abs_query('x_bi_min')) ;Zeile entfernt TD 21.12.05
;      xmax=FIX(recon_abs_query('x_bi_max')) ;Zeile entfernt TD 21.12.05
   ENDELSE

;   yanz=ymax-ymin+1
;   xanz=xmax-xmin+1 ;Zeile entfernt TD 21.12.05, wird nicht benutzt.

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
         one_slice, recopfad_host, sin_filename, sli_filename, FLOAT(zen), priority, silent=silent_now, /keep_pipe

         files(j)=recopfad+'/reco/'+scanname+numstr(i)+'.sli'
         j=j+1
      ENDIF
   ENDFOR
   rec_pipe_close
   IF (silent NE 1) THEN PRINT,'reconlogerz.pro END - ALL SLICES INSERTED.'

   IF WAIT THEN BEGIN
      waitforfiles, recopfad+'/reco/*', files
   ENDIF

END


;==============
PRO recon_init
END
