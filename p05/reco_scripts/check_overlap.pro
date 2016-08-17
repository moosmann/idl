
pro check_overlap, overlap_start=overlap_start, overlap_end=overlap_end

  binning=recon_abs_query('rawbin');2
  ;overlap_start=327
  ;overlap_end=328
  
  recopfad=recon_abs_query('recopfad')
  scanname=recon_abs_query('scanname')
  
  angles=36000/recon_abs_query('stepwinkel');2400
  
  na=recopfad + '/sino/' + scanname
  n=FINDFILE(na + '*.si_', co=co)
  IF co EQ 0 THEN BEGIN
    print,'No file exists!'
    print,'Suche: ',na+'*.si_'
    RETURN
  ENDIF

  i=co/2
  
  ;sino=read_dat('/asap3/petra3/gpfs/p05/2015/data/11000439/processed/phl_05a/sino/phl_05a00450.si_')
  sino=read_dat(n(i),/sil)
  sino1=sino(*,0:angles/2-1)
  sino2=reverse(sino(*,angles/2:angles-1),1)
  sino=make_array(3056,angles/2)
  for overlap=overlap_start, overlap_end,1 do begin; here 1 is step width, change e.g to 10
    sino(0:3056/binning-1,*)=(sino1)
    sino(3056/binning:2*3056/binning-1-overlap,*)=(sino2(overlap:*,*))

    for i=0, overlap do sino(3056/binning-1-overlap+i,*)=sino1(3056/binning-1-overlap+i,*)*(1-i/overlap)+sino2(i,*)*(i/overlap)

    test=att_fbp_quick(bin_1d(sino(0:-overlap,*),2),0,rot=!PI); binning of images change between 2 and 4
    if overlap eq overlap_start then s=size(test)

    ;tv, bytscl(test,-.01,.007)
    test2=congrid(test,s(1),s(2))
    ;write_tiff, '/asap3/petra3/gpfs/p05/2015/data/11000665/processed/bmc_nerve_biopsy3_a/sino/test_overlap/bmc_nerve_biopsy3_a00677_'+string(format='(I03)',overlap)+'.tif',test2, /float
    file_mkdir,recopfad+'/test/'
    write_tiff, recopfad+'/test/'+scanname+'_'+string(format='(I04)',overlap)+'.tif',test2, /float
   ;write_dat, test2,'I:\bmc_nervebiopsy\bmc_nerve_biopsy3_a00523_'+string(format='(I03)',overlap),'dat'
  endfor

end