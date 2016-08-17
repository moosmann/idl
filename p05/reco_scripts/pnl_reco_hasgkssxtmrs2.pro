; Imke Jan 2014

pro pnl_reco_hasgkssxtmrs
  rawbasepath='/scratch/hzgpp05ct1/hzg_p05_2013_10/10010202_pnl_raw_201310/'
  recobasepath='/xtmreco/tomodata/hzg_p05_2013_10_res/'
  
  ; start reco PNL31 PROBLEM ROH DATEN SCHON WEG....
  ;RecoScript, 'pnl31a', rawbasepath=rawbasepath, recobasepath=recobasepath, y0=0, y1=859, numangles=1200,$
    ;/loadcorrelation, /sinogramsexist
  
  ; start reco
  RecoScript, 'pnl32d', rawbasepath=rawbasepath, recobasepath=recobasepath, scanmode='360deg', y0=0, y1=699, $
      numangles=2400, /loadcorrelation
    
  
end