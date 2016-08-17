
Pro pnlrecosli

  recopath=recon_abs_query('recopfad')
  ;sliname=recopath+'/reco/'+recon_abs_query('scanname')+'_'
  sliname=recopath+'/reco/'+recon_abs_query('scanname')

  spawn,'mkdir '+recopath+'/reco'

  zmin=long(recon_abs_query('zmin'))
  zmax=long(recon_abs_query('zmax'))

  for i=zmin,zmax do begin
     s=schichtcombine(i,/float)
     write_dat,s,sliname+numstr(i),'sli',dummy
     if i mod 100 eq 0 then begin
        print,'schreibe ',dummy
     endif
  endfor
end
