pro jwi_reco_hzgcuda2075_360deg
  rawbasepath='/xtmscan/tomodata/hzg_p05_2013_09/jwi/'
  recobasepath='/xtmreco/tomodata/hzg_p05_2013_09_res/jwi_res/'
  ;rawbasepath='/scratch/hzgpp05ct1/hzg_p05_2013_09/jwi/'

; start reco

RecoScript, 'jwi_1309_s278R_abs_360deg_05b', rawbasepath=rawbasepath, recobasepath=recobasepath, $
              numangles=2400, /createnovol, /loadcorrelation, rawbin=2, scanmode = '360deg'

end 