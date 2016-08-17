pro RecoScript, name, rawbasepath=rawbasepath, recobasepath=recobasepath, rawbin = rawbin, $
                numangles = numangles, x0 = x0, x1 = x1, y0 = y0, y1 = y1,$
                manualreccenter = manualreccenter, loadcorrelation = loadcorrelation,$
                sinoimagerot = sinoimagerot, startcenter=startcenter, scripted = scripted, $
                createnotiff = createnotiff, createnovol = createnovol, volumebin = volumebin, $ 
                rec_center=rec_center, slope=slope, sinogramsexist = sinogramsexist, $
                scanmode = scanmode, overlap360degree = overlap360degree, $
                corrx00=corrx00, corrx01=corrx01, corrx10=corrx10, corrx11=corrx11,$
                reco_rot=reco_rot, noreco=noreco, man360overlap=man360overlap, createnormimages = createnormimages, $
                sinofilterthresh = sinofilterthresh 
    common reco_petra_common, scaninfo, recoinfo

    if n_elements(recobasepath) eq 0 then begin
        print, 'No recobasepath given.' & stop & endif
    if n_elements(rawbasepath) eq 0  then begin 
        print, 'No rawbasepath given.' & stop & endif
    
    if n_elements(rawbin) eq 0 then _rawbin = 2 else _rawbin = rawbin
    if n_elements(scanmode) eq 0          then  _scanmode = '180deg' else _scanmode = scanmode
    if n_elements(numangles) eq 0    then _numangles = 1200      else _numangles = numangles
    scaninfo = Log_ReadScanLog(name, recobasepath = recobasepath, rawbasepath = rawbasepath, rawbin = _rawbin, numangles = numangles, scanmode = _scanmode, /createlog)

    if n_elements(x0) eq 0           then _x0 = 0               else _x0 = x0 
    if n_elements(x1) eq 0           then _x1 = (scaninfo.img_xmax - scaninfo.img_xmin) / _rawbin  else _x1 = x1
    if n_elements(y0) eq 0           then _y0 = 0               else _y0 = y0
    if n_elements(y1) eq 0           then _y1 = (scaninfo.img_ymax - scaninfo.img_ymin+1) / _rawbin  else _y1 = y1
    if n_elements(sinoimagerot) eq 0 then _sinoimagerot = 0     else _sinoimagerot = sinoimagerot
    if n_elements(createnotiff) eq 0 then _createnotiff = 0     else _createnotiff = createnotiff
    if n_elements(createnovol) eq 0  then _createnovol = 0      else _createnovol = createnovol
    if n_elements(volumebin) eq 0    then _volumebin = 2        else _volumebin = volumebin
    if n_elements(manualreccenter) eq 0   then manualreccenter = 0
    if n_elements(loadcorrelation) eq 0   then loadcorrelation = 0
    if n_elements(scripted)        eq 0   then scripted = 0
    if n_elements(sinogramsexist)  eq 0   then sinogramsexist = 0
    if n_elements(sinofilterthresh) eq 0  then sinofilterthresh = 0
    if n_elements(startcenter) eq 0       then startcenter = 0
    if n_elements(rec_center) eq 0        then  _rec_center = 0 else _rec_center = rec_center
    if n_elements(overlap360degree) eq 0  then  _overlap360degree = -1 else _overlap360degree = overlap360degree
    if n_elements(slope) eq 0             then  _slope = 0       else _slope = slope
    if n_elements(corrx00) eq 0		  then corrx00 = 0
    if n_elements(corrx01) eq 0		  then corrx01 = 100
    if n_elements(corrx10) eq 0		  then corrx10 = (_y1-1) * _rawbin - 100
    if n_elements(corrx11) eq 0		  then corrx11 = (_y1-1) * _rawbin
    if n_elements(reco_rot) eq 0   then _reco_rot = 0    else _reco_rot = reco_rot
    if n_elements(noreco) eq 0     then _noreco = 0 else _noreco = noreco
    if n_elements(man360overlap) eq 0 then _man360overlap = 0 else _man360overlap = man360overlap
    if n_elements(createnormimages) eq 0 then _createnormimages = 0 else _createnormimages = createnormimages 
    if _reco_rot ne 0 then recon_abs_set, 'recostartangle', _reco_rot, /overwrite
    recon_abs_set, 'eff_pix_binned', scaninfo.eff_pix * _rawbin, /overwrite
    if loadcorrelation eq 0 then begin
        Corr_Prepare_RefImages, x00 = corrx00, x01 = corrx01, x10 = corrx10, x11 = corrx11, y0=_y0, y1=_y1, bin = _rawbin
        Corr_CalcCorrelation 
    endif else begin
        scaninfo = Log_RestoreScaninfo(name)
        recoinfo = Log_RestoreRecoinfo(name)
    endelse ; loadcorrelation eq 0

;    if _createnormimages eq 1 then begin
;        FILE_MKDIR, recobasepath+'/' + name +'/norm'
;        _path=recon_abs_query('recopfad')+'/norm/'+recon_abs_query('scanname')
;        for i = 0,(_numangles-1) do begin
;            tmp = Norm_CalcAbsImg(i, rawbin = 1, imagerot = _imagerot, /silent)
;            write_tiff,_path+numstr(i)+'.tif', tmp, /float
;        endfor ;i = 0,(_numangles-1) do begin
;    endif ;_createnormimages eq 1 then begin

    if not sinogramsexist then begin
        if _scanmode eq '180deg' then begin     
            Sino_SinogramCreation, rawbin = _rawbin, x0 = _x0, x1 = _x1, numangles = numangles, imagerot = _sinoimagerot
            Sino_FilterSino, thr = 0.0, /check ;,filtname='rescale_filterz',par1=5 
        endif ; _scanmode eq '180deg'                   rescale_filterz'
        if _scanmode eq '360deg' then begin
            recon_abs_set,'stepwinkel', 360. / numangles * 100
            Sino_SinogramCreation, rawbin = _rawbin, x0 = _x0, x1 = _x1, numangles = numangles, imagerot = _sinoimagerot
            Sino_FilterSino, thr = sinofilterthresh,/check
            if _man360overlap ne 0 then stop
            if _overlap360degree eq -1 then Sinograms360degree_Combine $
                                       else Sinograms360degree_Combine, overlap = _overlap360degree
        endif ;_scanmode eq '360deg' 
    endif ;sinogramsexist
    
    if _noreco ne 1 then begin
        if _rec_center eq 0 then begin
            if manualreccenter eq 0 then begin
                if scripted eq 0 then Reco_FindRotCenter, startcenter=startcenter $
                else Reco_FindRotCenter, startcenter=startcenter, /noabort, /ignorelimits
                _center = recon_abs_query('reczentrum')
                if _center ne -1 then begin
                    Reco_FillRecoPipe, /check, /wait 
                    Hist_Creation, /show
                    if _createnotiff eq 0 then Tiff_Make,/check
                    if _createnovol eq 0 then begin
                        recon_abs_set,'xmin_sin',0,/overwrite
                        recon_abs_set,'ymin_sin',0,/overwrite
                        recon_abs_set,'xmax_sin',fix((_x1-_x0)/_rawbin),/overwrite
                        recon_abs_set,'ymax_sin',fix((_y1-_y0)/_rawbin),/overwrite
                        recon_abs_set,'pixelsize',scaninfo.eff_pix * _rawbin
                        Vol_CreateVolume,bin= _volumebin, zrange = [fix(recon_abs_query('slrangemin')), fix(recon_abs_query('slrangemax'))]
                    endif ;_createnovol eq 0
                endif ;_center ne -1 
            endif ;manualreccenter eq 0
        endif else RecoScript_FromRecCenter, rec_center=_rec_center, slope=_slope,/keeplog,/maketiff ;_rec_center eq 0
    endif ;_nereco ne 0
end ;pro RecoScript


pro RecoScript_FromRecCenter, rec_center = rec_center, slope = slope, $
                              maketiff = maketiff, makevol = makevol, keeplog = keeplog, $
                              logfile = logfile, volumebin = volumebin, reco_rot = reco_rot
    common recon_abs_name, reconlogname
    if n_elements(keeplog) eq 0 $
        and n_elements(logfile) eq 0  then set_reconlog
    print, 1
    if n_elements(logfile) ne 0     then begin
        recon_abs_set, 'recopfad', logfile, /overwrite
        reconlogname = logfile
    endif
    if n_elements(rec_center) eq 0 then _rec_center = -1 else _rec_center = rec_center
    if n_elements(slope) eq 0      then _slope = 0       else _slope = slope
    if n_elements(volumebin) eq 0  then _volumebin = 2   else _volumebin = volumebin
    if n_elements(maketiff) eq 0   then _maketiff = 0    else _maketiff = 1
    if n_elements(makevol) eq 0    then _makevol = 0     else _makevol = 1
    if n_elements(volumebin) eq 0  then _volumebin = 2   else _volumebin = volumebin
    if n_elements(reco_rot) eq 0   then _reco_rot = 0    else _reco_rot = reco_rot
    if _reco_rot ne 0 then recon_abs_set, 'recostartangle', _reco_rot, /overwrite
    
    if _rec_center ne -1 $
        then Reco_FillRecoPipe, /check, /wait , rec_center =  _rec_center, slope = _slope $ 
        else Reco_FillRecoPipe, /check, /wait 
    Hist_Creation
    if _maketiff ne 0 then Tiff_Make, /check
    if _makevol ne 0  then begin 
        recon_abs_set,'pixelsize',recon_abs_query('eff_pix_binned')
        Vol_CreateVolume,bin= _volumebin, zrange = [fix(recon_abs_query('ymin')), fix(recon_abs_query('ymax'))]
    endif ; _makevol
end ;pro RecoScript_FromRecCenter


pro RecoScript_FindRecCenter, slices = slices, cen0 = cen0, cen1 = cen1, delta = delta, $
                              automatic  = automatic, keeplog = keeplog, logfile = logfile
    common recon_abs_name, reconlogname
    if n_elements(keeplog) eq 0 $
        and n_elements(logfile) eq 0  then set_reconlog
    if n_elements(logfile) ne 0     then begin
        recon_abs_set, 'recopfad', logfile, /overwrite
        reconlogname = logfile
    endif
    if n_elements(slices) eq 0      then _slices = [(fix(recon_abs_query('ymax'))+ fix(recon_abs_query('ymin')))/2] $
                                    else _slices = slices
    if n_elements(cen0) eq 0        then _cen0 = (fix(recon_abs_query('xmax_sin'))+ fix(recon_abs_query('xmin_sin')))/2 - 10$      
                                    else _cen0 = cen0
    if n_elements(cen1) eq 0        then _cen1 = (fix(recon_abs_query('xmax_sin'))+ fix(recon_abs_query('xmin_sin')))/2 - 10$
                                    else _cen1 = cen1
    if n_elements(delta) eq 0       then _delta = 1       else _delta = delta
    if n_elements(automatic) eq 0   then _automatic = 0   else _automatic = 1
    if _automatic eq 0 then begin
        for ii = 0, n_elements(_slices)-1 do $
            Reco_RecoWithVarRecCenter, sliceno = _slices[ii], _cen0, _cen1, _delta
    endif ;_automatic eq 0
    if _automatic eq 1 then begin 
        center_val = fltarr(n_elements(_slices))
        for ii = 0, n_elements(_slices)-1 do begin
            curr_sino = getsinmean(_slices[ii], 5)
            center_val[ii] = cor_iter(curr_sino, dtr_succ=dtr_succ, dtr_start=32, dtr_stop = 0.1)
        endfor
        print, center_val
        c0 = poly_fit(slices, center_val, 1)
        print, 'Starting rec center = ', c0[0]
        print, 'Slope               = ', c0[1]
    endif ;_automatic eq 1
end  ;pro RecoScript_FindRecCenter
  
