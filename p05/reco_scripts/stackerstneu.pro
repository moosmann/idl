;=================================================================================
PRO stackerst, bin=bi, xrange=xr, yrange=yr, zrange=zr, stackdir=stackdir, stacktype=stacktype, vgi=vgi

    schichtcomb=0

    IF N_ELEMENTS(stacktype) eq 0 THEN stacktype=1

    if stacktype eq 1 then begin
       stacktypebytes=1
       stacktypetypstr='b'
    endif else if stacktype eq 4 then begin
       stacktypebytes=4
       stacktypetypstr='f'
    endif else begin
       print,'Wrong data type -> retall'
       retall
    endelse
    IF N_ELEMENTS(recopfad) EQ 0 THEN recopfad=seppath(get_reconlog())
    PRINT, 'recopfad: '+recopfad
    scanname=recon_abs_query('scanname', default=sepfile(recopfad))

    IF N_ELEMENTS(stackdir) NE 0 THEN visualpfad=stackdir ELSE visualpfad=recopfad

    scananz=FIX(recon_abs_query('scananz', /noinput))  ;Returns 0 if no entry scananz exists.
    IF scananz GE 1 THEN BEGIN
      PRINT, 'Detected multiple scan. Reading from subdirectories.'
      schichtcomb=1
    ENDIF


    IF schichtcomb EQ 0 THEN BEGIN
      ;SINGLE SCAN - READ DATA FROM THE reconlog.txt IN CASE OF A SINGLE SCAN

      xmin=FIX(recon_abs_query('xmin_sin', logname=recopfad+'/reconlog.txt', /noinput))
      xmax=FIX(recon_abs_query('xmax_sin', logname=recopfad+'/reconlog.txt', /noinput))

      xrecsize=xmax-xmin+1
      yrecsize=xmax-xmin+1

      zmin=FIX(recon_abs_query('ymin_sin', logname=recopfad+'/reconlog.txt', /noinput))
      zmax=FIX(recon_abs_query('ymax_sin', logname=recopfad+'/reconlog.txt', /noinput))

   ENDIF ELSE BEGIN
     ;Combined scan:
     ;THIS READS FROM THE reconlog.txt FOR THE COMBINED SCANS.

      ;Detect the reconstruction size from the reconlog.txt file
      xrecsize=FIX(recon_abs_query('xmax')) - FIX(recon_abs_query('xmin')) + 1
      yrecsize=FIX(recon_abs_query('ymax')) - FIX(recon_abs_query('ymin')) + 1

      zmin=FIX(recon_abs_query('zmin'))
      zmax=FIX(recon_abs_query('zmax'))
   ENDELSE

   sinobin=FIX(recon_abs_query('sinobin'))
   rawbin=FIX(recon_abs_query('rawbin'))

   slice_xmin= 0
   slice_xmax= xrecsize - 1
   slice_ymin= 0
   slice_ymax= yrecsize - 1
   slice_zmin= zmin
   slice_zmax= zmax

   IF NOT KEYWORD_SET(xr) THEN xr=[slice_xmin,slice_xmax]
   IF NOT KEYWORD_SET(yr) THEN yr=[slice_ymin,slice_ymax]
   IF NOT KEYWORD_SET(zr) THEN zr=[slice_zmin,slice_zmax]
   IF NOT KEYWORD_SET(bi) THEN bi=1

   xsize=xr[1]-xr[0]+1 & xsb=xsize/bi
   ysize=yr[1]-yr[0]+1 & ysb=ysize/bi
   zsize=zr[1]-zr[0]+1 & zsb=zsize/bi

   pixelsize=FLOAT(recon_abs_query('pixelsize'))*bi

   megabyte=LONG(recon_abs_query('megabyte'))
   max_sp=LONG(LONG(megabyte)*1024*1024/xsb/ysb/stacktypebytes)
   IF max_sp GT zsb THEN max_sp=zsize
   get_lun,lun
   feldname=scanname+'_'+STRTRIM(bi,2)+'b_'+STRTRIM(xr[0],2)+'x'+STRTRIM(xr[1],2)+$
             '_'+STRTRIM(yr[0],2)+'y'+STRTRIM(yr[1],2)+'_'+STRTRIM(zr[0],2)+'z'+STRTRIM(zr[1],2)
   if not keyword_set(vgi) then begin
      openw, lun, visualpfad+'/'+feldname+(stacktype eq 1 ? '' : 'float')+'.dat'
      WRITEU, lun, 'PC_3_'+STRTRIM(STRUPCASE(stacktypetypstr))+'_' +STRTRIM(xsb,2)+ '_' +STRTRIM(ysb,2)+ '_' +STRTRIM(zsb,2)+ '_', 13b, 10b

      feld=(stacktype eq 1 ? BYTARR(xsb,ysb,max_sp) : FLTARR(xsb,ysb,max_sp))
      schicht=(stacktype eq 1 ? BYTARR(xsb*bi,ysb*bi,bi) : FLTARR(xsb*bi,ysb*bi,bi))
      FOR z=0,zsb-1 DO BEGIN
        IF z MOD (zsb/10) EQ 0 THEN BEGIN
           print,(zsb-z-1)*bi+zr[0]
        ENDIF
        FOR i=0,bi-1 DO BEGIN
          ;Read a slice.
          IF (schichtcomb NE 0) $
             THEN t=(stacktype eq 1 ? schichtcombine(zr[0]+z*bi+i,/tiff) :  schichtcombine(zr[0]+z*bi+i,/float))$
             ELSE t=(stacktype eq 1 ? READ_TIFF(visualpfad+'/tiff/'+scanname+numstr(zr[0]+z*bi+i)+'.tif') : READ_DAT(visualpfad+'/reco/'+scanname+numstr(zr[0]+z*bi+i)+'.sli',/silent))
          schicht(*,*,i)=t(xr[0]-slice_xmin:xr[0]-slice_xmin+xsb*bi-1,yr[0]-slice_ymin:yr[0]-slice_ymin+ysb*bi-1)
          WAIT,.05
        ENDFOR
        feld(*,*,z MOD max_sp)=REBIN(schicht,xsb,ysb,1)
        IF ((z MOD max_sp) EQ max_sp-1) OR (z EQ zsb-1) THEN BEGIN
           WRITEU,lun,feld(*,*,0:(z MOD max_sp))
        ENDIF
      ENDFOR
      CLOSE, lun  
      print, visualpfad+'/'+feldname+(stacktype eq 1 ? '' : 'float')+'.dat', ' erzeugt!'
   endif
; Erzeugung des VGI-Files
   OPENW, lun,  visualpfad+'/'+feldname+(stacktype eq 1 ? '' : 'float')+'.vgi'
    PRINTF, lun, '{volume1}
    PRINTF, lun, '[representation]'
    PRINTF, lun, 'size = ', xsb, ysb, zsb
    PRINTF, lun, 'datatype = unsigned integer'
    PRINTF, lun, 'bitsperelement = 8'
    PRINTF, lun, '[file1]'
    PRINTF, lun, 'RegionOfInterestStart = 0 0 0'
    PRINTF, lun, 'RegionOfInterestEnd = ', xsb-1, ysb-1, zsb-1
    PRINTF, lun, 'SkipHeader = ', 16+FIX(ALOG(xsb)/ALOG(10))+FIX(ALOG(ysb)/ALOG(10))+FIX(ALOG(zsb)/ALOG(10))
    PRINTF, lun, 'FileFormat = raw'
    PRINTF, lun, 'Size = ', xsb, ysb, zsb
    PRINTF, lun, 'Mirror = 0 0 1'
    PRINTF, lun, 'Name = ', feldname+'.dat'
    PRINTF, lun, 'Datatype = unsigned integer'
    PRINTF, lun, 'BitsPerElement = 8'
    PRINTF, lun, '[description]'
    PRINTF, lun, 'text = 0'

    PRINTF, lun, '{default}'
    PRINTF, lun, '[version]'
    PRINTF, lun, 'release = VGStudio 1.0 Beta (build 47)'

    PRINTF, lun, '{camera1}'
    PRINTF, lun, '[viewer]'
    PRINTF, lun, 'perspective = activated'
    PRINTF, lun, 'viewingangle = 30'
    PRINTF, lun, 'position = 0 -700 0'
    PRINTF, lun, 'oversampling = 1'
    PRINTF, lun, 'up = 0 0 1'
    PRINTF, lun, 'lookat = 0 0 0'
    PRINTF, lun, '[rendering]'
    PRINTF, lun, 'colormode = activated'
    PRINTF, lun, 'quickmode = activated'
    PRINTF, lun, 'algorithm = scatterhq'
    PRINTF, lun, '[stereo]'
    PRINTF, lun, 'status = NOT activated'
    PRINTF, lun, 'mode = redgreen'
    PRINTF, lun, 'swapping = NOT activated'
    PRINTF, lun, 'viewdistance = 700'
    PRINTF, lun, 'eyedistance = 10'

    PRINTF, lun, '{scene}'
    PRINTF, lun, '[viewer]'
    PRINTF, lun, 'perspective = activated'
    PRINTF, lun, 'viewingangle = 30'
    PRINTF, lun, 'position = 0 -700 0'
    PRINTF, lun, 'oversampling = 1'
    PRINTF, lun, 'up = 0 0 1'
    PRINTF, lun, 'lookat = 0 0 0'
    PRINTF, lun, '[light1]'
    PRINTF, lun, 'diffuse = 0.225 0.225 0.225 0.225'
    PRINTF, lun, 'position = 0 0 1'
    PRINTF, lun, 'status = activated'
    PRINTF, lun, 'specular = 0.3 0.3 0.3 0.3'
    PRINTF, lun, 'direction = 0 0 -1'
    PRINTF, lun, 'ambient = 0.225 0.225 0.225 0.225'
    PRINTF, lun, '[light2]'
    PRINTF, lun, 'diffuse = 0.225 0.225 0.225 0.225'
    PRINTF, lun, 'position = 0 0 1'
    PRINTF, lun, 'status = activated'
    PRINTF, lun, 'specular = 0.3 0.3 0.3 0.3'
    PRINTF, lun, 'direction = 0 0 -1'
    PRINTF, lun, 'ambient = 0.225 0.225 0.225 0.225
    PRINTF, lun, '[rendering]
    PRINTF, lun, 'background = 0 0 0 255
    PRINTF, lun, 'intensity = 0.5
    PRINTF, lun, 'colormode = activated
    PRINTF, lun, 'quickmode = activated
    PRINTF, lun, 'algorithm = scatterhq
    PRINTF, lun, '[stereo]
    PRINTF, lun, 'status = NOT activated
    PRINTF, lun, 'mode = redgreen
    PRINTF, lun, 'swapping = NOT activated
    PRINTF, lun, 'viewdistance = 700
    PRINTF, lun, 'eyedistance = 10

    PRINTF, lun, '{volumeprimitive4}
    PRINTF, lun, '[geometry]
    PRINTF, lun, 'clipplane1 = -1 0 0 ', xsb
    PRINTF, lun, 'clipbox = 0 0 0 ', xsb, ysb, zsb
    PRINTF, lun, 'position = 0 0 0'
    PRINTF, lun, 'status = visible
    PRINTF, lun, 'resolution = ',pixelsize, pixelsize, pixelsize
    PRINTF, lun, 'center = ', xsb/2., ysb/2., zsb/2.
    PRINTF, lun, 'scale = 1 1 1
    PRINTF, lun, 'ROTATE = 1 0 0 0 0.897212 0.4416 0 -0.4416 0.897212
    PRINTF, lun, '[volume]
    PRINTF, lun, 'volume = volume1
    PRINTF, lun, '[description]
    PRINTF, lun, 'text = ', feldname+'.dat [1]'
    PRINTF, lun, '[segment1]
    PRINTF, lun, 'opacityvalue = -1.81899e-012 2
    PRINTF, lun, 'status = activated
    PRINTF, lun, 'redvalue = 255 255
    PRINTF, lun, 'bluex = 0 255
    PRINTF, lun, 'greenvalue = 255 255
    PRINTF, lun, 'description = 0
    PRINTF, lun, 'opacityx = 0 255
    PRINTF, lun, 'redx = 0 255
    PRINTF, lun, 'greenx = 0 255
    PRINTF, lun, 'bluevalue = 255 255

    CLOSE, lun
    FREE_LUN, lun

    PRINT, visualpfad+'/'+feldname+(stacktype eq 1 ? '' : 'float')+'.vgi', ' erzeugt!'

    RETURN
END
