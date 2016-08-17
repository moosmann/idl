function Log_ReadPetraLogValue, was, long=long, double=dou, uint=uin, ulo=ulo, petra_current=petra_current
  common read_log_petra_common, log_content
  po=strpos(log_content.st,was)+strlen(was)
  erg=''
  if keyword_set(long) then erg=long(strmid(log_content.st,po)) $
  else if keyword_set(dou) then erg=double(strmid(log_content.st,po)) $
  else if keyword_set(uin) then erg=uint(strmid(log_content.st,po)) $
  else if keyword_set(ulo) then erg=ulong(strmid(log_content.st,po)) $
  else if keyword_set(petra_current) then begin
    po=strpos(log_content.st,'Statusserver Data')
    sth=strmid(log_content.st,po)
    sthpo=strpos(sth,'/PETRA/GLOBALS/#keyword/BeamCurrent')+strlen('/PETRA/GLOBALS/#keyword/BeamCurrent')
    sthpoend=strlen(sth)-1;strpos(sth,'tango:',sthpo)-1
    pe=strmid(sth,sthpo,sthpoend-sthpo-1)
    petra=strsplit(pe,string(10b),/extract,count=co)
    petra_current=dblarr(co)
    petra_time=ulonarr(co)
    petra_orgtime=ulonarr(co)
    for j=2,co-1 do begin ;changed j=0 to j=2
      ps=strsplit(petra[j],'@[]',/extract)
      petra_time[j]=ulong(ps[0])
      petra_current[j]=double(ps[1])
      petra_orgtime[j]=ulong(ps[2])
    endfor
    erg=create_struct('petra_time',petra_time,'petra_current',petra_current,'petra_orgtime',petra_orgtime)
  endif
  return,erg
end

function Log_ReadScanLog, dirname, recobasepath = recobasepath, rawbasepath = rawbasepath, $
                          rawbin=rawbin, numangles=numangles, createlog=createlog, scanmode = scanmode

   ; Log_ReadScanLog usage:
   ;   <dirname>       Name of the scan / folder
   ;   <recobasepath>  base path to reco folder *mandatory*
   ;   <rawbasepath>   base path to raw data folder *mandatory*
   ;   <rawbin>           binning setting for the reconstruction 
   ;                   (preset: rawbin = 2)
   ;   <numangles>     number of angle positions in the scan 
   ;                   (preset: numangles = 900)
   ;   <basepath>      basic path where data is to be found
   ;                   (preset: basepath = /scratch/hzgpp05ct1/hzg_p05_2013_06/)
   ;   <createlog>     flag for creation of logfile
   
  common read_log_petra_common, log_content
  common reco_petra_common, scaninfo, recoinfo
  
  if n_elements(rawbin) eq 0 then rawbin = 2
  if n_elements(numangles) eq 0 then numangles = 900
  ;if n_elements(baserecopath) eq 0 then baserecopath = '/xtmreco/tomodata/hzg_p05_2013_06_res/'
  ;if n_elements(basedatapath) eq 0 then basicpath = '/scratch/hzgpp05ct1/hzg_p05_2013_06/' else basicpath = basedatapath
  if n_elements(recobasepath) eq 0 then begin
    print, 'No recobasepath given.'
    stop
  endif
  if n_elements(rawbasepath) eq 0 then begin
    print, 'No rawbasepath given.'
    stop
  endif
  if n_elements(scanmode) eq 0 then _scanmode = '180deg' $
                               else _scanmode = scanmode
  
  
  print, rawbasepath + dirname
  print, rawbasepath+dirname+'/*scan.log'
  ;file_temp=file_search(rawbasepath+dirname+'*/*scan.log',count=co); changed as it didn't work for wzk
  file_temp=file_search(rawbasepath+dirname+'/*scan.log',count=co)
  if co ne 1 then begin
    print,"Several log-files (",strtrim(co,2),") found. Only one is allowed!"
    retall
  endif
  
  file_path=reform(file_dirname(file_temp))
  file_logname=reform(file_basename(file_temp))
  file_name=reform(strmid(file_logname,0,strlen(file_logname)-8))
  
  scan_rawbasepath=reform(file_dirname(file_dirname(file_path)))
  scan_beamtime=reform(file_basename(file_dirname(file_path)))
  scan_path=reform(file_basename(file_path))
  recopath = recobasepath; + scan_path
  
  fNamesDark=file_search(file_path+'/'+file_name+'*.dar',co=countDark)
  fNamesRef=file_search(file_path+'/'+file_name+'*.ref',co=countRef)
  fNamesImg=file_search(file_path+'/'+file_name+'*.img',co=countImg)
  print, "Found ", strtrim(countDark,2), " dark files, ", strtrim(countRef,2), " ref files, ", strtrim(countImg,2), " img files."
  countTotal=countRef+countDark+countImg
  ;countTotal=2094
  print, "Total number of files = ",strtrim(countTotal,2)
  fi=file_info(file_path+'/'+file_logname)
  ficont=bytarr(fi.size)
  openr, lun, file_path+'/'+file_logname, /get_lun
  readu,lun,ficont
  close,lun
  free_lun,lun
  
  file_temp=file_search(rawbasepath+dirname+'/*bild.log',count=co)
  ;file_temp=file_search(rawbasepath+dirname+'*/*bild.log',count=co)
  if co ne 1 then begin
    print,"Several or no bildlog-files (",strtrim(co,2),") found. Only one is allowed!"
    retall
  endif
  fi=file_info(file_temp[0])
  fibildcont=bytarr(fi.size)
  openr, lun, file_temp[0], /get_lun
  readu,lun,fibildcont
  close,lun
  free_lun,lun
  
  st=string(ficont)
  stpos=where(ficont eq 10b)
  log_content=create_struct('filepath',file_path,'filename',file_name,'st',st,'stpos',stpos)
  
  erg=create_struct( $
    'scan_rawbasepath',scan_rawbasepath,'scan_beamtime',scan_beamtime,'scan_path',scan_path,'scan_name',file_name,  $
    'scan_recopath', recopath, $
    'n_ref',Log_ReadPetraLogValue('n_ref=',/lo), $
    'n_dark',Log_ReadPetraLogValue('n_dark=',/lo), $
    'n_img',Log_ReadPetraLogValue('n_img=',/lo), $
    'n_angle',Log_ReadPetraLogValue('n_angle=',/lo), $
    'ref_count',Log_ReadPetraLogValue('ref_count=',/lo), $
    'img_bin',Log_ReadPetraLogValue('img_bin=',/lo), $
    'img_xmin',Log_ReadPetraLogValue('img_xmin=',/lo), $
    'img_xmax',Log_ReadPetraLogValue('img_xmax=',/lo), $
    'img_ymin',Log_ReadPetraLogValue('img_ymin=',/lo), $
    'img_ymax',Log_ReadPetraLogValue('img_ymax=',/lo), $
    'exptime',Log_ReadPetraLogValue('exptime=',/lo), $
    'ccd_pixelsize',Log_ReadPetraLogValue('ccd_pixsize:',/do), $
    'ccd_xsize',Log_ReadPetraLogValue('ccd_xsize:',/lo), $
    'ccd_ysize',Log_ReadPetraLogValue('ccd_ysize:',/lo), $
    'o_screen_changer',Log_ReadPetraLogValue('o_screen_changer:',/do), $
    'o_focus',Log_ReadPetraLogValue('o_focus:',/do), $
    'o_aperture',Log_ReadPetraLogValue('o_aperture:',/do), $
    'o_lens_changer',Log_ReadPetraLogValue('o_lens_changer:',/do), $
    'o_ccd_high',Log_ReadPetraLogValue('o_ccd_high:',/do), $
    'magn',Log_ReadPetraLogValue('magn:',/do), $
    'eff_pix',Log_ReadPetraLogValue('eff_pix:',/dou), $
    's_pos_x',Log_ReadPetraLogValue('pos_s_pos_x=',/dou), $
    's_pos_y',Log_ReadPetraLogValue('pos_s_pos_y=',/dou), $
    's_pos_z',Log_ReadPetraLogValue('pos_s_pos_z=',/dou), $
    's_stage_z',Log_ReadPetraLogValue('pos_s_stage_z=',/dou), $
    's_in_pos',Log_ReadPetraLogValue('s_in_pos=',/dou), $
    's_out_dist',Log_ReadPetraLogValue('s_out_dist=',/dou), $
    'stimg_name', strarr(countTotal),$
    'stimg_time_pre',ulonarr(countTotal),$
    'stimg_time_post',ulonarr(countTotal),$
    'stimg_angle',dblarr(countTotal),$
    'stimg_type',intarr(countTotal),$
    'stimg_avcurrent',dblarr(countTotal),$
    'stimg_avcurrentcalc',dblarr(countTotal),$
    'stimg_correfind',lonarr(countTotal),$
    Log_ReadPetraLogValue('petra_current',/petra_current))
    
  stbild=string(fibildcont)
  stbs=strsplit(stbild,', '+string(byte(10b)),/extract)
  
  for i=0, countTotal-1 do begin
    hna=strsplit(stbs[i*7+1],'/',co=co,/extract)
    hpre=strsplit(stbs(i*7+4),'@[],',/extract) ;hpre=strsplit(stbs(i*7+4),'@[],',/extract)
    hpost=strsplit(stbs(i*7+6),'@[],',/extract);hpost=strsplit(stbs(i*7+6),'@[],',/extract)
    erg.stimg_time_pre(i)=ulong(hpre(0))
    erg.stimg_time_post(i)=ulong(hpost(0))
    erg.stimg_avcurrent(i)=(float(hpre(1))+float(hpost(1)))/2
    
    erg.stimg_name[i]=strtrim(hna[co-1],2)
    erg.stimg_type[i]=(strpos(hna[co-1],'.dar') ge 1 ? -1 : $
      (strpos(hna[co-1],'.ref') ge 1 ? 0 : 1))
    erg.stimg_angle[i]=double(stbs[i*7+2])
    erg.stimg_avcurrentcalc[i]=total(interpol(erg.petra_current,erg.petra_time,$
      [erg.stimg_time_pre[i],erg.stimg_time_pre[i]/2+erg.stimg_time_post[i]/2,erg.stimg_time_post[i]]))/3
    if i mod 500 eq 0 then begin
      print,i
      wait,.1
    endif
  endfor
  erg.stimg_avcurrent(*)=erg.stimg_avcurrentcalc(*)
  scaninfo=erg
  if keyword_set(createlog) then begin
    ;stop
    FILE_MKDIR, recopath
    FILE_MKDIR, recopath+'/sino'
    create_reconlog, recopath+'/reconlog.txt'
    recon_abs_set,'recopfad',recopath
    recon_abs_set,'scanname',dirname
    recon_abs_set,'ymin',0
    recon_abs_set,'rawbin',rawbin
    recon_abs_set,'ymax',fix((erg.img_ymax-erg.img_ymin+1)/rawbin)-1
    recon_abs_set,'xmin',0
    recon_abs_set,'xmax',fix((erg.img_xmax-erg.img_xmin+1)/rawbin)-1
    recon_abs_set,'scanmode', _scanmode
    if _scanmode eq '180deg' then recon_abs_set,'stepwinkel', 180. / numangles * 100
    if _scanmode eq '360deg' then recon_abs_set,'stepwinkel', 360. / numangles * 100      
    recon_abs_set,'magn',erg.magn
    recon_abs_set,'sinobin',1
    recon_abs_set,'recopfad_host',''
    recon_abs_set,'recopfad_host_os','linux'
    recon_abs_set,'recostartangle',0
    recon_abs_set,'reconstructiontype',0
    recon_abs_set,'scmin',-0.01
    recon_abs_set,'scmax',0.05
    recon_abs_set,'scstep',1e-05
    recon_abs_set,'beamtime',scan_beamtime
    recon_abs_set,'tiffmin',-0.0002
    recon_abs_set,'tiffmax',0.0053
    recon_abs_set,'megabyte',500
    recon_abs_set,'s_pos_x',erg.s_pos_x
    recon_abs_set,'s_pos_y',erg.s_pos_y
    recon_abs_set,'s_high',erg.s_stage_z+erg.s_pos_z
    recon_abs_set,'eff_pix',erg.eff_pix
  endif
  return,erg
end


function Log_RestoreScaninfo, name
   common reco_petra_common, scaninfo, recoinfo
   ;cd, current = tmpdir
   restore, name + '_scaninfo_with_corr.sav', /relaxed_structure_assignment
   print, 'Restored scaninfo'
   return, scaninfo
end

function Log_RestoreRecoinfo, name
  common reco_petra_common, scaninfo, recoinfo
  ;cd, current = tmpdir
  restore, name + '_recoinfo_with_corr.sav', /relaxed_structure_assignment
  print, 'Restored recoinfo'
  return, recoinfo
end

function Log_GetLogContent
 common read_log_petra_common, log_content
 return, log_content
end