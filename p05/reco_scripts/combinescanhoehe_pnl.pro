
;**************************************************************
PRO combinescanhoehe

   IF N_ELEMENTS(old_mot_step) EQ 0 THEN old_mot_step=0

   PRINT, 'Grunddatenlesen'

;   recopfad=recon_abs_query('recopfad')
   IF N_ELEMENTS(recopfad) EQ 0 THEN recopfad=seppath(get_reconlog())

   n=FILE_SEARCH(recopfad+'/*/reconlog.txt', count=nscans_found)
   IF nscans_found EQ 0 THEN MESSAGE, 'ERROR: No scans found.'
   print, ''
   PRINT, 'found scans: ', nscans_found

   IF nscans_found NE 0 THEN BEGIN
      soheight=REPLICATE(0., nscans_found)
      FOR i=0, nscans_found-1 DO BEGIN
        name=STRMID(n(i), STRLEN(recopfad)+1, STRLEN(n(i))-STRLEN(recopfad)-14)
        winkelstrlaenge=FIX(recon_abs_query('winkelstrlaenge', logname=recopfad+'/'+name+'/reconlog.txt', /noinput))

        ;at the time winkelstrlaenge was 5, we used a different motor step value
        IF (winkelstrlaenge EQ 5) AND (old_mot_step NE 1) THEN BEGIN
          PRINT, 'winkelstrlaenge=5 detected. Use Keyword /old_mot_step. RETALL.'
          RETALL
        ENDIF
        conversion_in_mm=(old_mot_step EQ 1 ? .005 : 1.) ; steps per um   alt bei wsl 5 sind 200MS = 1000um
        print, 'conversion_in_mm', conversion_in_mm
        soheight(i)=FLOAT(recon_abs_query('s_high', logname=recopfad+'/'+name+'/reconlog.txt'))*conversion_in_mm
       ;OUTPUT OF SCAN HEIGHTS
       PRINT, i, ': ', name, ' = ', STRTRIM(soheight(i), 2),' mm'
     ENDFOR
   ENDIF


   PRINT, 'RETRIEVING DATA FOR CALCULATION OF scan_pixesize (effective pixel size) FROM FILE ', n(0)

   magn=recon_abs_query('magn', logname=n(0), /noinput)
   IF magn EQ '' THEN magn=0. ELSE magn=FLOAT(magn)
   PRINT, 'magnification: ', magn
   sinobin=recon_abs_query('sinobin', logname=n(0), /noinput)
   IF sinobin EQ '' THEN sinobin=1 ELSE sinobin=FLOAT(sinobin)
   PRINT, 'sinobin: ', sinobin

   ;bin=recon_abs_query('bin', logname=n(0), /noinput)
   ;IF bin EQ '' THEN bin=1 ELSE bin=FLOAT(bin)
   ;PRINT, 'bin: ', bin
   ;recon_abs_set, 'bin', bin  ;write the value of bin into the new reconlog.txt

   rawbin=recon_abs_query('rawbin', logname=n(0), /noinput)
   IF rawbin EQ '' THEN rawbin=1 ELSE rawbin=FLOAT(rawbin)
      PRINT, 'rawbin: ', rawbin
   
   ;rawbin=1 ; fixed rawbin for further processing
   recon_abs_set, 'rawbin', rawbin  ;write the value of rawbin into the new reconlog.txt

   PRINT, 'scan_pixelsize NOW IN mm UNITS'
   ;scan_pixelsize=FLOAT(recon_abs_query('eff_pix_binned'));!!! changed for standard rawbin2 in primary reconstruction
   scan_pixelsize=recon_abs_query('eff_pix_binned', logname=n(0), /noinput)

   PRINT, 'scan_pixel_size (mm): ', scan_pixelsize

   scananz=FIX(recon_abs_query('scananz', default=nscans_found))

   scan_name=STRARR(scananz)
   scan_xmin=LONARR(scananz)
   scan_xmax=LONARR(scananz)
   scan_ymin=LONARR(scananz)
   scan_ymax=LONARR(scananz)
   scan_zmin=LONARR(scananz)
   scan_zmax=LONARR(scananz)
   scan_height=FLTARR(scananz)
   scan_sposx=FLTARR(scananz)
   scan_sposy=FLTARR(scananz)

   ulapp=INTARR(scananz)
   diffx=INTARR(scananz)
   diffy=INTARR(scananz)

   IF nscans_found EQ scananz THEN BEGIN
      sorth=SORT(soheight)    ; SORTING: sorth CONTAINS THE INDICES FOR soheight VALUES IN
                              ; INCREASING ORDER.
      FOR i=0, scananz-1 DO $
         recon_abs_set,'scan_name_'+numstr(i,sl=2),$
            STRMID(n(sorth(i)), STRLEN(recopfad)+1, STRLEN(n(sorth(i)))-STRLEN(recopfad)-14)
   ENDIF

   PRINT, 'Sortierte Eingabe der Scans: Angefangen bei kleinstem Wert von s_pos_high. (Oberster Teil der Probe)'
   PRINT, 'DATEN AUS DEN EINZELNEN reconlog.txt WERDEN GELESEN BZW. ERGAENZT.'

   FOR i=0, scananz-1 DO BEGIN
      scan_name[i]=recon_abs_query('scan_name_'+numstr(i,sl=2))
      PRINT, scan_name(i)
;      PRINT, scan_name(i), '  sinobin:', recon_abs_query('sinobin', logname=recopfad+'/'+scan_name[i]+'/reconlog.txt', /noinput)
;      sinb=recon_abs_query('sinobin', logname=recopfad+'/'+scan_name[i]+'/reconlog.txt', /noinput)
;      IF sinb EQ '' THEN sinb=1 ELSE sinb=FIX(sinb)
;      xmin=FIX(recon_abs_query('xmin', logname=recopfad+'/'+scan_name[i]+'/reconlog.txt', /noinput))

      xmin=recon_abs_query('x_bi_min', logname=recopfad+'/'+scan_name[i]+'/reconlog.txt', /noinput)
      IF xmin EQ '' THEN xmin=recon_abs_query('xmin_sin', logname=recopfad+'/'+scan_name[i]+'/reconlog.txt', /noinput)
      IF xmin EQ '' THEN xmin=recon_abs_query('xmin', logname=recopfad+'/'+scan_name[i]+'/reconlog.txt')
      scan_xmin[i]=FIX(xmin)
      xmax=recon_abs_query('x_bi_max', logname=recopfad+'/'+scan_name[i]+'/reconlog.txt', /noinput)
      IF xmax EQ '' THEN xmax=recon_abs_query('xmax_sin', logname=recopfad+'/'+scan_name[i]+'/reconlog.txt', /noinput)
      IF xmax EQ '' THEN xmax=recon_abs_query('xmax', logname=recopfad+'/'+scan_name[i]+'/reconlog.txt')
      scan_xmax[i]=FIX(xmax)

;      scan_xmin[i]=0   ;Eingef�gt 16.0.05 TD, sinobin und rawbin vorher nicht ber�cksichtigt.
;      scan_xmax[i]=(scan_xmax[i] - scan_xmin[i] + 1)/sinobin/rawbin - 1


     ;gleiche Werte f�r die y-Richtung �bernehmen.
      scan_ymin[i]=scan_xmin[i]
      scan_ymax[i]=scan_xmax[i]


      ;z-Richtung (Rotationsachse) (Achtung: die Schichten beginnen quasi immer bei z=0)
      ;zmin=FIX(recon_abs_query('slrangemin', logname=recopfad+'/'+scan_name[i]+'/reconlog.txt', /noinput))
      zmin=FIX(recon_abs_query('ymin', logname=recopfad+'/'+scan_name[i]+'/reconlog.txt', /noinput))
      scan_zmin[i]=zmin
      ;zmax=FIX(recon_abs_query('slrangemax', logname=recopfad+'/'+scan_name[i]+'/reconlog.txt', /noinput))
      zmax=FIX(recon_abs_query('ymax', logname=recopfad+'/'+scan_name[i]+'/reconlog.txt', /noinput))
      scan_zmax[i]=zmax

      scan_height[i]=FLOAT(recon_abs_query('s_high',logname=recopfad+'/'+scan_name[i]+'/reconlog.txt'))
      scan_sposx[i]=FLOAT(recon_abs_query('s_pos_x',logname=recopfad+'/'+scan_name[i]+'/reconlog.txt'))
      scan_sposy[i]=FLOAT(recon_abs_query('s_pos_y',logname=recopfad+'/'+scan_name[i]+'/reconlog.txt'))

      IF old_mot_step EQ 1 THEN BEGIN
        scan_height[i]=FLOAT(recon_abs_query('pos_s_high',logname=recopfad+'/'+scan_name[i]+'/reconlog.txt'))*5./1000. ; 200MS = 1000um
        scan_sposx[i]=FLOAT(recon_abs_query('pos_s_pos_x',logname=recopfad+'/'+scan_name[i]+'/reconlog.txt'))*5./1000. ; 200MS = 1000um
        scan_sposy[i]=FLOAT(recon_abs_query('pos_s_pos_y',logname=recopfad+'/'+scan_name[i]+'/reconlog.txt'))*5./1000. ; 200MS = 1000um
      ENDIF


      PRINT, numstr(i,sl=2),':',scan_name[i] $
           , ' x,y,z = [', STRTRIM(scan_xmin[i],2), ',', STRTRIM(scan_xmax[i],2), '], ['$
           , STRTRIM(scan_ymin[i],2), ',', STRTRIM(scan_ymax[i],2), '], [' $
           , STRTRIM(scan_zmin[i],2), ',', STRTRIM(scan_zmax[i],2) $
           , ']   Hoehe = ',STRTRIM(scan_height[i] - scan_height[0], 2),' mm'
   ENDFOR


   PRINT, ''
   PRINT, 'Berechnung der Ueberlappung'
   PRINT, ''

   FOR i=1, scananz-1 DO BEGIN
      ; dist = HOEHENVERSATZ DER SCANS IN PIXELN
      DIST=FIX((scan_height[i]-scan_height[i-1])/scan_pixelsize+.5)

      ulapp[i-1]=scan_zmax[i-1]-scan_zmin[i]-DIST
;      ulapp[i-1]=(scan_zmax[i-1]-scan_zmin[i])/sinobin/rawbin-DIST ; Neu eingef�gt TD 16.9.05

      IF ulapp[i-1] LT 0 THEN BEGIN
         PRINT, 'KEIN UEBERLAPP DER DATENSAETZE!'
         STOP
      ENDIF

      diffx[i-1]=FIX((scan_sposx[i-1]-scan_sposx[i])/scan_pixelsize+.5)
      diffy[i-1]=FIX((scan_sposx[i-1]-scan_sposx[i])/scan_pixelsize+.5)
      print, numstr(i-1,sl=2),' mit ',numstr(i,sl=2)$
         ,'   ',numstr(i-1,sl=2),' [',STRTRIM(scan_zmin[i-1],2),',',STRTRIM(scan_zmax[i]-DIST,2)$
         ,'] = ',numstr(i,sl=2),' [',STRTRIM(DIST+scan_zmin[i-1],2),',',STRTRIM(scan_zmax[i],2)$
         ,']   size = ',STRTRIM(ulapp[i-1],2)
      print,'                    diffx,diffy = ',STRTRIM(diffx[i-1],2),',',STRTRIM(diffy[i-1],2)
   ENDFOR

   neu_scan_zmin=scan_zmin
   neu_scan_zmax=scan_zmax
   neu_ulapp=ulapp
   neu_diffx=diffx
   neu_diffy=diffy

   FOR i=0, scananz-2 DO BEGIN
      PRINT, 'PruefeUeberlapp ',scan_name[i],' mit ',scan_name[i+1]

      IF KEYWORD_SET(typint) THEN typstr='fix' ELSE typstr='tiff'

      scan_scalmin=[FLOAT(recon_abs_query(typstr+'min',logname=recopfad+'/'+scan_name[i]+'/reconlog.txt')), FLOAT(recon_abs_query(typstr+'min',logname=recopfad+'/'+ scan_name[i+1]+'/reconlog.txt'))]
      scan_scalmax=[FLOAT(recon_abs_query(typstr+'max',logname=recopfad+'/'+scan_name[i]+'/reconlog.txt')), FLOAT(recon_abs_query(typstr+'max',logname=recopfad+'/'+scan_name[i+1]+'/reconlog.txt'))]
      scan_size=[scan_xmax[i]-scan_xmin[i]+1,scan_xmax[i+1]-scan_xmin[i+1]+1]

      IF KEYWORD_SET(typint) THEN typrecon=2 ELSE typrecon=3

     ; CALL THE WIDGET sl_search
      erg=sl_search(nampath=scan_name[i:i+1],$
                zmin=scan_zmin[i:i+1],$
                zmax=scan_zmax[i:i+1],$
                ulapp=ulapp[i],$
                diffx=diffx[i],$
                diffy=diffy[i],$
                scalmin=MIN(scan_scalmin),scalmax=MAX(scan_scalmax),$
                scaltyprecon=typrecon,$
                binning=MAX([MIN(scan_size)/500,1]) )

      neu_scan_zmin[i]=erg[0]
      neu_scan_zmax[i+1]=erg[1]
      neu_ulapp[i]=erg[2]
      neu_diffx[i]=erg[3]
      neu_diffy[i]=erg[4]
   ENDFOR

xmin=0&xmax=0&ymin=0&ymax=0&zmin=0&zmax=-1
xoff=0&yoff=0

FOR i=scananz-1,0,-1 DO BEGIN
  recon_abs_set,'ulapp_'+numstr(i,sl=2),neu_ulapp[i]
  recon_abs_set,'xsize_'+numstr(i,sl=2), scan_xmax[i] - scan_xmin[i] + 1
  recon_abs_set,'ysize_'+numstr(i,sl=2), scan_ymax[i] - scan_ymin[i] + 1
  recon_abs_set,'diffx_'+numstr(i,sl=2),neu_diffx[i]
  recon_abs_set,'diffy_'+numstr(i,sl=2),neu_diffy[i]
  recon_abs_set,'zmin_'+numstr(i,sl=2),neu_scan_zmin[i]
  recon_abs_set,'zmax_'+numstr(i,sl=2),neu_scan_zmax[i]
  xoff=xoff+neu_diffx[i]
  yoff=yoff+neu_diffy[i]
  IF xoff lt xmin THEN xmin=xoff
  IF yoff lt ymin THEN ymin=yoff
  IF xoff+scan_xmax[i]-scan_xmin[i] GT xmax THEN xmax=xoff+scan_xmax[i]-scan_xmin[i]
  IF yoff+scan_ymax[i]-scan_ymin[i] GT ymax THEN ymax=yoff+scan_ymax[i]-scan_ymin[i]
  zmax=zmax+neu_scan_zmax[i]-neu_scan_zmin[i]+1-neu_ulapp[i]
ENDFOR

recon_abs_set,'xmin',0
recon_abs_set,'xmax',xmax-xmin
recon_abs_set,'xoff',-xmin
recon_abs_set,'ymin',0
recon_abs_set,'ymax',ymax-ymin
recon_abs_set,'yoff',-ymin
recon_abs_set,'zmin',0
recon_abs_set,'zmax',zmax

print,'Gesamt x=[0,',STRTRIM(xmax-xmin,2),'] y=[0,',STRTRIM(ymax-ymin,2),'] z=[0,',STRTRIM(zmax,2),']'

END
