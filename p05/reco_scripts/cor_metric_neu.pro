;================================================
FUNCTION cor_metric, sino, tr, metric=metric, binfac=binfac, $
  reco=reco, given_reco=given_reco, $
   r0_in=r0_in, dx=dx, dy=dy, filter=filter, par_filter=par_filter, $
  interpol3=interpol3, gauss=gauss, linear=linear, normalize=normalize, $
  verbose=verbose
;+
;Returns the quality for a sinogram and a certain position of the
;rotation axis tr. tr can be an array. The parameter metric defines
;which quality measure is to be used.
;It is more efficient to give all tr at the same time, than one by
;one. More importantly also the performance will change if the
;range of tr changes for most quality measures. Therefore one
;should only compare values that have been obtained in a single
;call to cor_metric.
;Gauss is necessary to supress oscillating effects in the metrics values.
;These oscillations are probably caused by different interpolation
;for different positions of the filtered sinogram relative to the grid. Can also
;be overcome by finer grid (dx=... dy=...)?
;
;See publication: T. Donath et al., J.Opt.Soc.Am. A, 23(5), pp. May 2006.
;-

;Parameters
; metric     : An array containing the names of the metrics to be calculated.
; tr         : Array of values for the center of rotation tr, for which the metrics are calculated.
; normalize  : St this Keyword explicitly to zero if no normalization of metrics IA and IN is desired


  IF N_ELEMENTS(metric) EQ 0 THEN MESSAGE, "ERROR: 'metric' undefined."
  IF N_ELEMENTS(verbose) EQ 0 THEN verbose=0
  IF N_ELEMENTS(normalize) EQ 0 THEN normalize=1

  ;Set flag to indicate, that reconstruction is required.
  ;All metrics defined now require the reconstruction of the sinogram. Might
  ;be not the case for future metrics operating with the sinogram directly.
  calc_reco=0
  IF is_member('IN', metric) THEN calc_reco=1
  IF is_member('IA', metric) THEN calc_reco=1
  IF is_member('H', metric) THEN calc_reco=1

  ;Number of metric parameters
  n_metric=N_ELEMENTS(metric)
  ;Number of centers of rotation
  n_tr=N_ELEMENTS(tr)
  IF N_ELEMENTS(tr) EQ 0 THEN MESSAGE, "ERROR: 'tr' undefined."

  s=SIZE(sino)
  IF s(0) NE 2 THEN MESSAGE, "ERROR: 'sino' is not two dimensional."
  xsize=s(1)
  xsize_sino=s(1)
  ysize=s(2)

  ;The size of a reconstruction bin in relation to a sinogram bin.
  IF N_ELEMENTS(binfac) EQ 0 THEN binfac=1


  ;Initialzie flag 'given_reco'. If it is set, the data given in 'reco' will be used
  ;for calculation of the metric values.
  IF N_ELEMENTS(given_reco) EQ 0 THEN given_reco_flag=0 ELSE given_reco_flag=1
  IF given_reco_flag THEN BEGIN
    calc_reco=0
    s_reco=SIZE(given_reco)
    CASE s_reco(0) OF
     3 : reco=given_reco
     2 : reco=REFORM(given_reco, s_reco(1), s_reco(2), 1)
     ELSE : MESSAGE, 'ERROR: given_reco.'
    ENDCASE
    s_reco=SIZE(reco)
    IF n_tr NE s_reco(3) THEN MESSAGE, 'ERROR: number of elements in tr and given_reco mismatch.'
  ENDIF


  ;Reconstruct the sinogram data.
  IF calc_reco EQ 1 THEN BEGIN

    ;Determine the parameter for the reconstruction

    ;Calculate radius 'r0_min' needed for the reconstruction grid
    pixels_before_tr=tr
    pixels_after_tr=xsize - 1 - tr
    r0_min=MAX(CEIL([pixels_before_tr, pixels_after_tr]))
   ;Check 'r0_in'. The user can specify an inreased reconstruction diameter
    IF N_ELEMENTS(r0_in) NE 0 THEN BEGIN
      IF (LONG(r0_in) LT r0_min) THEN MESSAGE, "ERROR: 'r0_in' must be greater than or equal to 'r0_min': " + STRING(r0_min)
      r0=LONG(r0_in)
    ENDIF ELSE r0=r0_min
    IF verbose THEN PRINT, 'Using r0: ', r0

    ;zeropad the sinogram data, such that the projections fully cover the reconstruction grid
    pixels_before_extra= CEIL(SQRT(2.)*r0 - MIN(pixels_before_tr))
    pixels_after_extra= CEIL(SQRT(2.)*r0 - MIN(pixels_after_tr))
    sino_pad=zeropad(sino, xfront=pixels_before_extra, xback=pixels_after_extra)
    xsize=xsize + pixels_before_extra + pixels_after_extra
    tr_pad=tr + pixels_before_extra

    ;resolution of the reconstruction grid
    IF N_ELEMENTS(dx) EQ 0 THEN dx=1.
    IF N_ELEMENTS(dy) EQ 0 THEN dy=1.

    ;Linear interpolation in the backprojection
    IF N_ELEMENTS(linear) EQ 0 THEN linear=1

    ;No reconstruction filter used in Fourier space
    IF N_ELEMENTS(filter) EQ 0 THEN filter=''

    ;Convolve the sinograms in real space by a Gauss function of sigma=1. Stabilizes against noise.
    IF N_ELEMENTS(gauss) EQ 0 THEN gauss=1.
    IF (gauss GT 0) THEN sino_pad=sino_convol_gauss(sino_pad, [1., gauss])


    start_time=SYSTIME(0, /seconds)

    ;Call the reconstruction procedure for all tr
    FOR i_tr=0, n_tr - 1 DO BEGIN

      reco_tmp=reconstruct(sino_pad, tr_pad(i_tr), size=1, par_size=r0, $
                      filter=filter, par_filter=par_filter, dx=dx, dy=dy, $
                      silent=1, interpol3=0, linear=linear)

      ;Normalize with 'binfac' to obtain reconstructions with comparable
      ;attenuation coefficient. Hereby, the total mass (m0) is not conserved!
      reco_tmp=reco_tmp/binfac^2.

      ;Create an array for all reconstructions
      IF (i_tr EQ 0) THEN BEGIN
        s=SIZE(reco_tmp)
        reco=REPLICATE(0., s(1), s(2), n_tr)
        mask=bytarr(s(1),s(2))
        for ix=0,s(1)-1 do for iy=0,s(2)-1 do mask(ix,iy)=(((ix-s(1)/2.)^2+(iy-s(2)/2.)^2) lt ((s(1)^2)/4.*1^2))
      ENDIF

      reco(*,*,i_tr)=reco_tmp;*mask

      ;Check if the user pressed abort key
      test_user_abort, ['q', 'Q']

    ENDFOR
    end_time=SYSTIME(0, /seconds)
    IF verbose EQ 2 THEN PRINT, 'Reconstruction time: ', FIX(end_time - start_time), ' seconds.'

  ENDIF

  start_time=SYSTIME(0, /seconds)

  ;Caculate the metric values
  qual=REPLICATE(DOUBLE(0.), n_tr, n_metric)
  FOR i_metric=0, n_metric - 1 DO BEGIN

    WAIT, .1

    CASE metric(i_metric) OF
    ;Image Entropy (H)
    'H' : BEGIN
          start_time=SYSTIME(0, /seconds)
          ;Calculate an average attenuation coefficient estimate 'aa'.
          ;Use ABS(sino) in calculation, since sino is allowed to be <=0
          aa=MEAN(ABS(sino))/xsize_sino
          ;Set the resolution parameter 'h' to one percent of 'aa'.
          h=.01*aa
          FOR i_tr=0, n_tr-1 DO qual(i_tr, i_metric)=arr_entropy(reco(*,*,i_tr), h)
          end_time=SYSTIME(0, /seconds)
          IF verbose EQ 2 THEN PRINT, 'Metric H: ', (end_time - start_time), ' seconds.'
        END
    ;Integrated Negativity (IN)
    'IN' : BEGIN
          start_time=SYSTIME(0, /seconds)

          ;Calculate the average sinogram mass 'm0' for normalization
          IF normalize EQ 1 THEN BEGIN
            m0=MEAN(TOTAL(sino,1))
            IF m0 LE 0 THEN MESSAGE,  'ERROR: m0 LE 0.'
          ENDIF ELSE m0=1.

          ;Multiply by binfac to correct for reduced number of pixels in binned reconstruction.
          FOR i_tr=0, n_tr-1 DO qual(i_tr, i_metric)=image_negativity(reco(*,*,i_tr))*binfac^2./m0
          end_time=SYSTIME(0, /seconds)
          IF verbose EQ 2 THEN PRINT, 'Metric IN: ', (end_time - start_time), ' seconds.'
        END
    ;Integrated Absolute (IA)
    'IA' : BEGIN
          start_time=SYSTIME(0, /seconds)

          ;Calculate the average sinogram mass 'm0' for normalization
          IF normalize EQ 1 THEN BEGIN
            m0=MEAN(TOTAL(sino,1))
            IF m0 LE 0 THEN MESSAGE,  'ERROR: m0 LE 0.'
          ENDIF ELSE m0=1.

          ;Multiply by binfac to correct for reduced number of pixels in binned reconstruction.
          FOR i_tr=0, n_tr-1 DO qual(i_tr, i_metric)=image_total_abs(reco(*,*,i_tr))*binfac^2./m0 ;- 1 ;ATTENTION: The -1 is missing in the JOSA Publication!!!
          end_time=SYSTIME(0, /seconds)
          IF verbose EQ 2 THEN PRINT, 'Metric IA: ', (end_time - start_time), ' seconds.'
        END
;Muss noch getestet werden, bevor die Zeilen wieder einkommentiert werden.
;    ;Cross correlation for 360deg data
;    'cc360deg' : BEGIN
;          start_time=SYSTIME(0, /seconds)
;
;          FOR i_tr=0, n_tr-1 DO qual(i_tr, i_metric)=-cc360deg(sino, tr(i_tr))
;          end_time=SYSTIME(0, /seconds)
;          IF verbose EQ 2 THEN PRINT, 'Metric cc360deg: ', (end_time - start_time), ' seconds.'
;        END


    ELSE : MESSAGE, 'ERROR: Undefined metric: ' +  metric(i_metric)
    ENDCASE

  ENDFOR
  RETURN, qual
END
