;====================================================================
FUNCTION cor_iter, sino, metric=metric, $
  tr0_start=tr0_start, dtr_start=dtr_start, dtr_stop=dtr_stop, $
  tr0_succ=tr0_succ, dtr_succ=dtr_succ, binfac_succ=binfac_succ, $
  verbose=verbose, demo=demo, save_dir=save_dir, status=status, $
  ignorelimits = ignorelimits

;+
;This procedure iteratively determines the best center of rotation for
;the reconstruction of sinogram data in 'sino'.
;
;The procedure calls cor_metric for five centers of rotation:
;tr=[tr0-2*dtr, tr0-1*dtr, tr0, tr0+1*dtr, tr0+2*dtr] and the 'metric' parameter.
;The tr of the reconstruction with minimum metric value is used as
;tr0 for the next iteration.
;The sinogram is binned by the factor 'binfac' for dtr>0. No binning
;is applied in the angular direction.
;
;Input Parameters
; sino       : The sinogram data.
; metric     : Name of the metric to be used in cor_metric.pro (default: 'IA')
; tr0_start  : First estimate of the center of rotation (default: x-center of sinogram)
; dtr_start  : First resolution of tr-values. (default: x-size of sinogram / 5)
; dtr_stop   : Final resolution of tr-values. Exactly at this reslution the iteration ends. (default: .05)
; verbose    : Set verbose=0 to suppress output to the log window. (default: 1)
; demo       : Set /demo to produce output of all reconstruction to the screen.
; save_dir   : Set e.g. save_dir='C:\temp\' to save the reconstructions.

;Output Parameters
; tr0_succ   : Result for the center of rotation of last successful iteration. This is the return parameter.
; dtr_succ   : Resolution 'dtr' at last successful iteration.
; binfac_succ: Binning factor 'binfac' at last successful iteration.
; status     : status=0, finished successfully
;            : status=1, not successful but user accepted iteration result
;            : status=-1  ;not successfull
;-


  IF N_ELEMENTS(metric) EQ 0 THEN metric='IA'
  IF N_ELEMENTS(verbose) EQ 0 THEN verbose=1
  IF N_ELEMENTS(plot) EQ 0 THEN plot=0
  IF N_ELEMENTS(demo) EQ 0 THEN demo=0
  IF N_ELEMENTS(ignorelimits) EQ 0 THEN _ignorelimits = 0 else _ignorelimits = 1
  
  
  s_sino=SIZE(sino)
  IF s_sino(0) NE 2 THEN MESSAGE, 'ERROR: Not a 2D array: sino'
  xs_sino=s_sino(1)
  ys_sino=s_sino(2)

  status=-1  ;not successfull (yet)

   IF verbose THEN PRINT,  "This is cor_iter.pro. To abort press 'q'."

  ;Initialize parameters for first iteration

   ;First resolution "dtr"
   dtr_start_max=xs_sino/5.
   IF N_ELEMENTS(dtr_start) NE 0 THEN BEGIN
     IF (dtr_start LE 0) THEN MESSAGE, 'ERROR: dtr_start below zero.'
     IF (dtr_start GT dtr_start_max) THEN MESSAGE, 'ERROR: dtr_start GT dtr_start_max: ' + STRING(dtr_start_max)
     dtr=dtr_start
   ENDIF ELSE BEGIN
     ;Calculate the next lower power of 2 for first_dtr_max and use as resolution
     dtr=2^(FLOOR(ALOG(dtr_start_max)/ALOG(2)))
   ENDELSE

   ;Last resolution, default 1./20. bin. For metric 'cc360deg' use .5 as last resolution
   IF N_ELEMENTS(dtr_stop) EQ 0 THEN dtr_stop=.05
   IF (metric EQ 'cc360deg') AND (dtr_stop LT .5) THEN BEGIN
     PRINT, 'WARNING: Changing dtr_stop to .5 for metric: ' + metric
     dtr_stop=.5
   ENDIF

   ;First center of roation estimate tr0; default is the middle of the array.
   IF N_ELEMENTS(tr0_start) NE 0 THEN BEGIN
     IF (tr0_start LT 0) THEN MESSAGE, 'ERROR: tr_start below zero.'
     IF (tr0_start GE xs_sino) THEN MESSAGE, 'ERROR: tr_start outside sinogram, maximum allowed: ' + STRING(xs_sino)
     tr0=tr0_start
   ENDIF ELSE tr0=(xs_sino - 1)/2.

   ;Print first line of iteration result table
   IF verbose THEN BEGIN
     PRINT, "Using metric: " + metric
     PRINT, '         tr0    /         dtr    /    binfac '
     PRINT, '-----------------------------------------------------------------------------------------'
   ENDIF



  ;Initialize variables storing the values tr0, dtr0 of the last successful iteration step.
  tr0_succ=-1
  binfac_succ=-1
  dtr_succ=-1

  done=0
  ;iteration counter
  i_iter=0

  ;ITERATION LOOP--------------------
  REPEAT BEGIN
    ;increase iteration counter
    i_iter++

    ;Set the binning factor "binfac" for this resolution
    IF dtr GE 1 THEN binfac=FIX(dtr) ELSE binfac=1



    ;Check if the resolution matches the binning for resolutions above 1.
    ;If it does not, this will not be a problem generally, but it can influence
    ;the result, when the metric values oscillates with one bin period.
    IF (dtr GE 1) AND (dtr NE binfac) THEN BEGIN
      PRINT, 'WARNING: Using a resolution, which does not match the binning factor. '
      PRINT, '(Use dtr_start values, which are a power of 2 to avoid this.)'
    ENDIF

   ;Create the binned sinogram in 'sino_bin', mulitpy by binfac to conserve total mass
    sino_bin=bin2d(sino, binfac, 1, /add_zero)*binfac
    s_sino_bin=SIZE(sino_bin)
    xs_sino_bin=s_sino_bin(1)

    ;Calculate the position of "tr0" in the binned sinogram: "tr0_bin"
    tr0_bin=(tr0 - (binfac - 1)/2.)/binfac
    ;and binned resolution
    dtr_bin=dtr/binfac

IF metric EQ 'cc360deg' THEN tr0_bin=ROUND(2*tr0_bin)/2.



    ;Calculate "tr_bin", the positions at which the metrics will be calculated for first time
    tr_bin=tr0_bin + (INDGEN(5) - 2)*dtr_bin

    ;Check the tr_bin parameters (can be too low or too high, when, the user allowed this. Not possible in automatic mode.
    IF MIN(tr_bin) LT 0 THEN BEGIN
      tr0_bin=tr0_bin - MIN(tr_bin)
      PRINT, 'Minimum tr_bin value too low, correcting.'
    ENDIF
    IF MAX(tr_bin) GT (xs_sino_bin - 1) THEN BEGIN
      tr0_bin=tr0_bin - (MAX(tr_bin) - (xs_sino_bin - 1))
      PRINT, 'Minimum tr_bin value too high, correcting.'
    ENDIF

    ;Recalculate "tr_bin", the positions at which the metrics will be calculated
    tr_bin=tr0_bin + (INDGEN(5) - 2)*dtr_bin


    w=WHERE((tr_bin LT 0) OR (tr_bin GT (xs_sino_bin - 1)), count)
    IF count NE 0 THEN MESSAGE, 'ERROR: cor_iter calculated tr_bin values out of range.'

    ;Calculate the corresponding ubinned coordinate
    tr=tr_bin*binfac + (binfac-1)/2.

    ;Output: Print the resolution parameters for this step
    IF verbose THEN BEGIN
      PRINT, STRING(tr0, FORMAT='(F15.8)') + ' /' + STRING(dtr, FORMAT='(F15.8)') + ' /' + STRING(binfac, FORMAT='(I8)')   ;+ ' ( ' + STRJOIN(STRING(tr)) + ' )'
    ENDIF


    ;Call cor_metric.pro to calculate the metric values q
    q=cor_metric(sino_bin, tr_bin, metric=metric, binfac=binfac, reco=reco)

    ;If /demo mode, then create windows with reconstruction results.
    IF (demo EQ 1) AND (N_ELEMENTS(reco) NE 0) THEN winn, reco


    IF N_ELEMENTS(save_dir) NE 0 THEN BEGIN
      IF dir_exists(save_dir) THEN BEGIN
        FOR i_file=0, 4 DO BEGIN
          filename=save_dir + 'cor_iter_r_' + NUMSTR(i_iter, slen=3) + '_' + STRTRIM(i_file, 2) + '_' + STRTRIM(tr0, 2) + '_' + STRTRIM(dtr, 2) + '_' + NUMSTR(binfac) + '.dat'
          ;Increase the reconstructed images by 'infac'
          incfac=binfac
          write_dat, bin2d(REFORM(reco(*,*,i_file)), -incfac), filename
        ENDFOR
        filename_q=save_dir + 'cor_iter_q_' + NUMSTR(i_iter, slen=3) + '_' + STRTRIM(tr0, 2) + '_' + STRTRIM(dtr, 2) + '_' + NUMSTR(binfac) + '.dat'
        write_dat, [[tr], [q]], filename_q    ;wr_ascii, arr2txt(tr, q), filename_txt
      ENDIF ELSE PRINT, "WARNING: Data not saved, 'save_dir' not found: " + save_dir
    ENDIF


    ;Determine the minimum of q
    qmin_index=global_minimum(q, atside=atside, conv_sigma=conv_sigma, conv_arr=conv_arr, silent=1)
    IF conv_sigma NE 0 THEN PRINT, "WARNING: Using convolution to find unique minimum, 'sigma': ", conv_sigma

    ;Plot the data and make the plot window active
    IF plot THEN BEGIN
      WSET, 0
      WSHOW
      PLOT, tr, q, /ynozero, psym=-4
      IF conv_sigma NE 0 THEN OPLOT, tr, conv_arr, linestyle=1
      IF N_ELEMENTS(save_dir) NE 0 THEN IF dir_exists(save_dir) THEN BEGIN
        filename=save_dir + 'cor_iter_plot_' + NUMSTR(i_iter, slen=3) +'.eps'
        xmin=tr(0) - dtr
        xmax=tr(4) + dtr
        ymin=MIN(q) - .1*(MAX(q) - MIN(q))
        ymax=MAX(q) + .1*(MAX(q) - MIN(q))
        PLOTPS, tr, q, xrange=[xmin, xmax], yrange=[ymin, ymax], xtitle='tr', ytitle='Q_'+metric, psym=-4, filename=filename
      ENDIF
    ENDIF

    ;Abort, when no minimum found  OR
    ;minimum only found with convolution and at highest or lowest tr value.
    IF (qmin_index EQ -1) OR ((atside NE 0) AND (conv_sigma NE 0)) THEN BEGIN

      ;Plot the data and make the plot window active
      WSET, 0
      WSHOW
      PLOT, tr, q, /ynozero, psym=-4
      IF conv_sigma NE 0 THEN OPLOT, tr, conv_arr, linestyle=1

      PRINT, 'WARNING: No minimum found at resolution: ', dtr
      PRINT, "Did not reach final resolution 'dtr_stop': " + STRING(dtr_stop, FORMAT='(F15.8)')
      PRINT, '-----------------------------------------------------------------------------------------'
      PRINT, '  optimum tr    /         dtr    /    binfac '
      PRINT, STRING(tr0_succ, FORMAT='(F15.8)') + ' /' + STRING(dtr_succ, FORMAT='(F15.8)') + ' /' + STRING(binfac_succ, FORMAT='(I8)')
      PRINT, 'Leaving cor_iter.pro.'
      RETURN, tr0_succ
    ENDIF


    ;Flag allowing change of resolution
    decrease_resolution=1

    IF (atside NE 0) and (_ignorelimits eq 0) THEN BEGIN
      IF atside EQ -1 THEN PRINT, 'WARNING: Minimum found at lower limit of tr values.'
      IF atside EQ +1 THEN PRINT, 'WARNING: Minimum found at uppper limit of tr values.'
;      PRINT, 'WARNING: Minimum found at, qmin_index: ', qmin_index
      PRINT, 'Do you want to repeat the iteration step with this minimum? [Y]es, [S]top, [C]ancel.'
      waitkeys, ['y', 's', 'c'], key1
      IF STRUPCASE(key1) EQ 'Y' THEN BEGIN
        PRINT, 'Continuing.'
      ENDIF ELSE BEGIN
        qmin_index=-1
        PRINT, "Did not reach final resolution 'dtr_stop': " + STRING(dtr_stop, FORMAT='(F15.8)')
        PRINT, '-----------------------------------------------------------------------------------------'
        PRINT, '  optimum tr    /         dtr    /    binfac '
        PRINT, STRING(tr0_succ, FORMAT='(F15.8)') + ' /' + STRING(dtr_succ, FORMAT='(F15.8)') + ' /' + STRING(binfac_succ, FORMAT='(I8)')
        PRINT, 'Leaving cor_iter.pro'
        IF STRUPCASE(key1) EQ 'C' THEN RETALL
        status=1 ; not successful but user accepted iteration result
        RETURN, tr0_succ
      ENDELSE

      ;initiate next iteration at same resolution
      decrease_resolution=0
      tr0=tr(qmin_index)
      tr0=MAX([tr0, 2*dtr], index)
      IF index NE 0 THEN PRINT, 'WARNING: tr0 too low. Corrected to minimum allowed value.'
      tr0=MIN([tr0, xs_sino - 1 - 2 * dtr], index)
      IF index NE 0 THEN PRINT, 'WARNING: tr0 too high. Corrected to maximum allowed value.'
    ENDIF

    ;Assign the best tr estimate to tr0 for next iteration
    tr0=tr(qmin_index)

    ;Store result of this successful iteration step
    tr0_succ=tr0
    binfac_succ=binfac
    dtr_succ=dtr


    ;Decrease the resolution for next iteration
    IF decrease_resolution THEN BEGIN
      IF (dtr EQ dtr_stop) THEN done=1
      dtr=dtr/2.
      IF (dtr LT dtr_stop) THEN dtr=dtr_stop
    ENDIF

  ENDREP UNTIL (done EQ 1)

  status=0 ;finished successfully

  IF verbose THEN BEGIN
    PRINT, 'Final resolution.'
    PRINT, '-----------------------------------------------------------------------------------------'
    PRINT, '  optimum tr    /         dtr    /    binfac '
    PRINT, STRING(tr0_succ, FORMAT='(F15.8)') + ' /' + STRING(dtr_succ, FORMAT='(F15.8)') + ' /' + STRING(binfac_succ, FORMAT='(I8)')
    PRINT, 'Leaving cor_iter.pro'
  ENDIF

  ;----------------------------------
  RETURN, tr0_succ
END
