pro overlap_test_erz, overlap=overlap,boarder=boarder

    if n_elements(boarder) eq 0 then _boarder=0 else _boarder=boarder
    
    recopfad=recon_abs_query('recopfad')
    scanname=recon_abs_query('scanname')
    
    na=recopfad + '/sino/' + scanname
    n=FINDFILE(na + '*.si_', co=co)
    IF co EQ 0 THEN BEGIN
      print,'Kein File vorhanden!'
      print,'Suche: ',na+'*.si_'
      RETURN
    ENDIF
    
    i=co/2
    s=read_dat(n(i),/sil)
    si=SIZE(s)
    sxsize=si(1)
    sysize=si(2)
    print,'sx :',sxsize,' ,sy :',sysize
    
    sl=s(*,0:sysize/2-1)
    help,sl
    sr=s(0:sxsize-1-_boarder,sysize/2:sysize-1)
    help,sr
    window,xs=sxsize,ys=sysize/2
    loadct,15
    tv,bytscl(sl,0,2),0,0&wait,1
    ;loadct,0
    tv,rotate(bytscl(sr,0,2),5),sxsize-overlap+_boarder,0   
    wshow
    draw_box,[sxsize-overlap+_boarder-10,sxsize-overlap+_boarder+10,0,sysize-1]
    rec_center=(sxsize-overlap+sxsize)/2+1
    print,'rec_center :',rec_center
end