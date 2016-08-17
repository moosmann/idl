
;=============================
PRO Hist_Creation, histo, x, step=step, show=show, circle_mask=circle_mask

;+
; CREATE A HISTOGRAM FROM ALL sli. FILES.
;-

   IF NOT KEYWORD_SET(show) THEN show=0

   IF NOT KEYWORD_SET(step) THEN step=1
   IF N_ELEMENTS(recopfad) EQ 0 THEN recopfad=seppath(get_reconlog())
;   recopfad=recon_abs_query('recopfad')
   scanname=recon_abs_query('scanname')
   datapath=recopfad+'/reco/'+scanname+'*.sli'
   n=FINDFILE(datapath, count=numcount)

   scmin=FLOAT(recon_abs_query('scmin')) ;, default=-.005))
   scmax=FLOAT(recon_abs_query('scmax'))  ;, default=.010))
   scstep=FLOAT(recon_abs_query('scstep')) ;, default=.00001))

   x=LINDGEN(long((scmax-scmin)/scstep)+1)*scstep+scmin; Use locations=x in HISTOGRAM below, to replace this
   IF numcount EQ 0 THEN BEGIN
      PRINT, 'No File to process on in: ', datapath
      RETURN
   ENDIF ELSE PRINT, numcount, ' .sli-files found.'

  histo=REPLICATE(0l, N_ELEMENTS(x))

  IF N_ELEMENTS(circle_mask) EQ 0 THEN circle_mask=0
  mask=0


  FOR i=0, numcount-1, step DO BEGIN
     tmp=read_dat(n(i),/silent)
     IF circle_mask THEN BEGIN
       IF N_ELEMENTS(tmp) NE N_ELEMENTS(mask) THEN BEGIN
         s=SIZE(tmp)
         xsize=s(1)
         ysize=s(2)
         mask=circle(xsize, ysize, (xsize-1)/2., (xsize-1)/2.,(xsize-1)/2.)
         wmask=WHERE(mask EQ 1)
       ENDIF
       tmp=tmp(wmask)
     ENDIF
     histo=histo+HISTOGRAM(tmp, MIN=scmin,MAX=scmax,binsize=scstep)
     IF i MOD 50 EQ 0 THEN BEGIN
        print, 'histerst.pro: ' + n(i)
        WAIT,.1
     ENDIF
  ENDFOR

  write_dat, histo, recopfad+'/histogram.dat'
  write_dat, x, recopfad+'/histox.dat'

  ;write a txt file of the histogram data. (Useful for import in other plotting software.)
  hist2txt

  IF show THEN begin
     h=histo
     w0=where(h eq max(h))
     h[w0[0]]=(h[w0[0]-1]+h[w0[0]+1])/2
     w1=where(h gt max(h)/20000.)
     PLOT, x, histo, xr=[x[min(w1)],x[max(w1)]],/ylog
     wshow
     wshow
  endif
  RETURN
END
