;===========================================================================
FUNCTION read_dat, name, extension, wname, silent=silent, nocheck=nocheck, $
  HELP=HELP, filename_out=filename_out, header_info=header_info

;+
; READS VARIABLES WHICH WERE SAVED WITH write_dat.
; THIS VERSION IS SYSTEM INDEPENDENT (sunos, PC (Win32, Linux)).
; IF name IS NOT GIVEN A FILE SELECTION DIALOG OPENS.
;
;filename_out RETURNS THE FILENAME.
;-

  ON_ERROR, 2

  CATCH, Error_status
  ;This statement begins the error handler:
  IF Error_status NE 0 THEN BEGIN
;   PRINT, 'Error index: ', Error_status
    PRINT, 'Error message: ', !ERROR_STATE.MSG
    CATCH, /CANCEL
    IF N_ELEMENTS(lun) NE 0 THEN BEGIN
      CLOSE,lun
      FREE_LUN,lun
    ENDIF
    IF (nocheck EQ 1) THEN RETURN, -1
    CATCH, /CANCEL
    MESSAGE, 'Leaving read_dat.pro with Error.'
  ENDIF

  ;Stop checking header after
  max_header_size=10000

  IF KEYWORD_SET(HELP) THEN BEGIN
    print, 'Function Read_Dat, Name[, Extension[, WNAME]]'
    RETURN,0
  ENDIF

  IF NOT KEYWORD_SET(silent) THEN silent=0
  IF N_ELEMENTS(nocheck) EQ 0 THEN nocheck=0

  IF N_PARAMS(0) EQ 0 THEN BEGIN
    name=DIALOG_PICKFILE()
    IF name EQ '' THEN RETURN, 0
    nocheck=1
  ENDIF

  ; Bestimmung des Filenamens
  IF N_PARAMS(0) GT 1 THEN BEGIN
    filename=name+'.'+Extension
    flist=FINDFILE(filename+'*')
    flist=flist(SORT(flist))
    filename=flist(N_ELEMENTS(flist) - 1)
  ENDIF ELSE filename=name


;IF (NOT nocheck) THEN BEGIN
;   allname=FINDFILE(filename+'*',count=count)
;
;   IF count EQ 0 THEN BEGIN
;      print, 'Filename '+ filename + ' does not exist!'
;      wname='NoFileError'
;       RETURN,0
;   ENDIF
;
;   filename=SORT(filename)
;   filename=allname(count-1)
;ENDIF ELSE allname=name


  ;Search for 13b 10b signature at the end of the header
  eintrag=''
  filename_out=filename
  OPENR, lun, /GET_LUN, filename
  by1=0b
  by2=0b
  READU, lun, by1, by2
  char_count=0
  WHILE NOT ((by1 EQ 13b) AND (by2 EQ 10b)) DO BEGIN
    eintrag=eintrag + STRING(by1)
    by1=by2
    READU,lun,by2
    char_count=char_count+1
    IF char_count GT max_header_size THEN MESSAGE, 'No valid header found.'
  ENDWHILE

  ;Determine the starting position of the entries between '_'
  seppos=STRSPLIT(eintrag, '_', length=length, count=count)

  comp=STRMID(eintrag, seppos(0), length(0))
  dimension=FIX(STRMID(eintrag, seppos(1), length(1)))
  typ=STRMID(eintrag, seppos(2), length(2))
  ;Rest of entry is header_info
  IF (count GT 3+dimension) THEN header_info=STRMID(eintrag, seppos(3+dimension), STRLEN(eintrag) - seppos(3+dimension)) ELSE header_info=''

;pos=STRPOS(eintrag,'_',0)
;comp=STRMID(eintrag,0,pos)
;pos1=STRPOS(eintrag,'_',pos+1)
;dimension=FIX(STRMID(eintrag, pos+1, pos1 - pos - 1))
;pos=pos1
;pos1=STRPOS(eintrag,'_',pos+1)
;typ=STRMID(eintrag,pos+1,pos1-pos-1)
;pos=pos1

  IF dimension EQ 0 THEN BEGIN
    CASE typ OF
      'B' : DATA=0b
      'I' : DATA=0
      'U' : DATA=uint(0)
      'L' : DATA=0l
      'F' : DATA=0.
      'D' : DATA=0d
      'C' : DATA=COMPLEX(0.,0.)
      ELSE: BEGIN
              MESSAGE,'Incorrect data format!'
;             GOTO, ende
            END
     ENDCASE
  ENDIF ELSE BEGIN
    d=LONARR(7)+1
    FOR i=0, dimension-1 DO BEGIN
      d(i)=LONG(STRMID(eintrag, seppos(3+i), length(3+i)))
;      pos1=STRPOS(eintrag,'_',pos+1)
;      d(i)=LONG(STRMID(eintrag,pos+1,pos1-pos-1))
;      pos=pos1
    ENDFOR

    ;WAS KOMMT DENN HIER? DAS KANN DOCH GAR NICHT PASSIEREN, BEDINGUNG NIE ERF�LLT. WAS SOLL DIE KR�CKE?
    IF d(dimension-1) EQ -1 THEN BEGIN
      d(dimension-1)=1;
      CASE typ OF
        'B' : DATA=   BYTARR(d(0),d(1),d(2),d(3),d(4),d(5),d(6)) ; Byte
        'I' : DATA=   INTARR(d(0),d(1),d(2),d(3),d(4),d(5),d(6)) ; Integer
        'U' : DATA=   UINTARR(d(0),d(1),d(2),d(3),d(4),d(5),d(6)) ; UInteger
        'L' : DATA=   LONARR(d(0),d(1),d(2),d(3),d(4),d(5),d(6)) ; Long Integer
        'F' : DATA=   FLTARR(d(0),d(1),d(2),d(3),d(4),d(5),d(6)) ; Float
        'D' : DATA=   DBLARR(d(0),d(1),d(2),d(3),d(4),d(5),d(6)) ; Double
        'C' : DATA=COMPLEXARR(d(0),d(1),d(2),d(3),d(4),d(5),d(6)) ; Complex
        ELSE: BEGIN
                MESSAGE,'  wrong datatype !! only numerical arrays allowed!'
;               GOTO, ende
              END
      ENDCASE
      d[dimension-1]=0
      WHILE NOT EOF(lun) DO BEGIN
         READU,lun,data
         d[dimension-1]=d[dimension-1]+1
      ENDWHILE
      CLOSE,lun
      openr,lun,filename
      READU,lun,by1,by2
      WHILE NOT ((by1 EQ 13b) AND (by2 EQ 10b)) DO BEGIN
        Eintrag=Eintrag+STRING(by1)
        by1=by2
        READU,lun,by2
      ENDWHILE
    ENDIF

    CASE typ OF
      'B' : DATA=   BYTARR(d(0),d(1),d(2),d(3),d(4),d(5),d(6)) ; Byte
      'I' : DATA=   INTARR(d(0),d(1),d(2),d(3),d(4),d(5),d(6)) ; Integer
      'U' : DATA=   UINTARR(d(0),d(1),d(2),d(3),d(4),d(5),d(6)) ; Integer
      'L' : DATA=   LONARR(d(0),d(1),d(2),d(3),d(4),d(5),d(6)) ; Long Integer
      'F' : DATA=   FLTARR(d(0),d(1),d(2),d(3),d(4),d(5),d(6)) ; Float
      'D' : DATA=   DBLARR(d(0),d(1),d(2),d(3),d(4),d(5),d(6)) ; Double
      'C' : DATA=COMPLEXARR(d(0),d(1),d(2),d(3),d(4),d(5),d(6)) ; Complex
      ELSE: BEGIN
              MESSAGE,'  wrong datatype !! only numerical arrays allowed!'
              ;GOTO, ende
            END
    ENDCASE
  ENDELSE

  READU, lun, data
  CLOSE, lun
  FREE_LUN, lun

  IF (comp EQ 'PC') AND (!VERSION.OS EQ 'sunos') THEN BEGIN
    CASE typ OF
      'B' :                            ;Byte
      'I' : BYTEORDER, data, /SSWAP    ;Integer
      'U' : BYTEORDER, data, /SSWAP    ;Integer
      'L' : BYTEORDER, data, /LSWAP    ;Long Integer
      'F' : BYTEORDER, data, /LSWAP    ;Float
      'D' : BYTEORDER, data, /L64SWAP  ;Double
      'C' : BYTEORDER, data, /L64SWAP  ;Complex
       ELSE: BEGIN
               MESSAGE,'  wrong datatype !! only numerical arrays allowed!'
               GOTO, ende
             END
    ENDCASE
  ENDIF

  IF (comp EQ 'SUN') AND ((!VERSION.OS EQ 'Win32') OR (!VERSION.OS EQ 'linux')) THEN BEGIN
    CASE typ OF
      'B' :                            ;Byte
      'I' : BYTEORDER, data, /SSWAP    ;Integer
      'U' : BYTEORDER, data, /SSWAP    ;Integer
      'L' : BYTEORDER, data, /LSWAP    ;Long Integer
      'F' : BYTEORDER, data, /LSWAP    ;Float
      'D' : BYTEORDER, data, /L64SWAP  ;Double
      'C' : BYTEORDER, data, /L64SWAP  ;Complex
      ELSE: BEGIN
              MESSAGE,'  wrong datatype !! only numerical arrays allowed!'
              GOTO, ende
            END
    ENDCASE
  ENDIF


  IF (N_PARAMS(0) lt 3) AND (silent EQ 0) THEN print, filename, '  read' $
                    ELSE wname=filename

ende:
  RETURN, data
END
