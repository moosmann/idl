;IDL> ri=obj_new('rec_insert','hasyw04.desy.de')
; IF ri EQ obj_new() THEN STOP   ;leeres socket - fehler aufgetreten.
;IDL> for i=0,1 do begin ri->sino,'hasyw04.desy.de','hasyw03.desy.de','/hasyw03/x1/felix/desy2003c_res/ris01a_pps/sino/r01a00501.sin',567.0,10.,.5,1&$
;ri->slice,'hasyw03.desy.de','/hasyw03/x1/felix/desy2003c_res/ris01a_pps/r01a0'+strtrim(i,2)+'test.sli','F',0.,0.,0,0,0,0&ri->go
;IDL> obj_destroy,ri



PRO rec_insert__DEFINE
    struct = {rec_insert, rs:OBJ_NEW(), recservmaster:'', eintrag:PTR_NEW()}
END

FUNCTION rec_insert::INIT, recservmaster
self.recservmaster=recservmaster
IF self.rs EQ OBJ_NEW() THEN self.rs=OBJ_NEW('recsocket',recservmaster,8070)
IF self.rs NE OBJ_NEW() THEN self.eintrag=PTR_NEW(OBJ_NEW('rec_insert_auftrag'))
RETURN, self.rs NE OBJ_NEW()
END

PRO rec_insert::sino,nachricht_rechner,sino_rechner_name,sino_datei_name,sino_datei_name_err,center,startangle,recontype,forder,ffrequenz,priority
(*(self.eintrag)).recrechner=nachricht_rechner
(*(self.eintrag)).sino_rechner_name=sino_rechner_name
(*(self.eintrag)).sino_datei_name=sino_datei_name
(*(self.eintrag)).sino_datei_name_err=sino_datei_name_err
(*(self.eintrag)).startangle=startangle
(*(self.eintrag)).recontype=recontype
(*(self.eintrag)).center_offset=center
(*(self.eintrag)).forder=forder
(*(self.eintrag)).ffrequenz=ffrequenz
(*(self.eintrag)).priority=priority
RETURN
END

PRO rec_insert::slice,slice_rechnername,slice_datei_name,slice_datei_name_err,typ,sc_min,sc_max,xmin,xmax,ymin,ymax
n=OBJ_NEW('rec_insert_sliceeintrag')
n.slice_rechner_name=slice_rechnername
n.slice_datei_name=slice_datei_name
n.slice_datei_name_err=slice_datei_name_err
n.typ=typ
n.sc_min=sc_min
n.sc_max=sc_max
n.xmin=xmin
n.xmax=xmax
n.ymin=ymin
n.ymax=ymax
n.next=(*(self.eintrag)).slice_eintrag
(*(self.eintrag)).slice_eintrag=PTR_NEW(n)
RETURN
END

PRO rec_insert::go
IF self.rs NE OBJ_NEW() THEN BEGIN
   (self.rs)->wr,'i'
   (self.rs)->wr,(*(self.eintrag))->write()
;print,'Eintrag'
;print,(*(self.eintrag))->write()

   (*(self.eintrag)).recrechner=''
   (*(self.eintrag)).sino_rechner_name=''
   (*(self.eintrag)).sino_datei_name=''
   (*(self.eintrag)).sino_datei_name_err=''
   (*(self.eintrag)).center_offset=''
   (*(self.eintrag)).startangle=0
   (*(self.eintrag)).recontype=2
   (*(self.eintrag)).forder=0
   (*(self.eintrag)).ffrequenz=0.
   (*(self.eintrag)).priority=0
   e=(*(self.eintrag)).slice_eintrag
   WHILE (e NE PTR_NEW()) DO BEGIN
      h=e
      e=(*e).next
      PTR_FREE, h
   ENDWHILE
   (*(self.eintrag)).slice_eintrag=PTR_NEW()
ENDIF

RETURN
END

PRO rec_insert::CLEANUP
  IF self.rs NE OBJ_NEW() THEN (self.rs)->wr,'e'
  OBJ_DESTROY,self.rs
  e=(*(self.eintrag)).slice_eintrag
  WHILE (e NE PTR_NEW()) DO BEGIN
     h=e
     e=(*e).next
     PTR_FREE, h
  ENDWHILE
RETURN
END


PRO rec_insert_sliceeintrag__DEFINE
    struct = {rec_insert_sliceeintrag, slice_rechner_name:'', slice_datei_name:'',slice_datei_name_err:'', typ:' ',$
              sc_min:0.,sc_max:0.,xmin:0,xmax:0,ymin:0,ymax:0,next: PTR_NEW()}
END

PRO rec_insert_auftrag__DEFINE
    struct = {rec_insert_auftrag, recrechner:'', sino_rechner_name:'', sino_datei_name:'',sino_datei_name_err:'',$
              center_offset:0., startangle:0, recontype:0, forder:0., ffrequenz:0., slice_eintrag: PTR_NEW(),$
              nachricht_rechner:'',nachricht_id:0l, status:0., $
              startauftrag:0l, startrecon:0l, endrecon:0l, endauftrag:0l, meldung:'',$
              priority:0}
END

PRO rec_insert_auftrag::CLEANUP
    WHILE self.slice_eintrag NE PTR_NEW() DO BEGIN
       h=self.slice_eintrag
       self.slice_eintrag=self.slice_eintrag.next
       OBJ_DESTROY, h
    ENDWHILE
RETURN
END

FUNCTION rec_insert_auftrag::write
   snull='(null)'
   n=STRTRIM(self.recrechner,2)&erg='recrechner: '+(n EQ '' ? snull : n)+STRING(13b)+STRING(10b)
   n=STRTRIM(self.sino_rechner_name,2)&erg=erg+'sino_rechner_name: '+(n EQ '' ? snull : n)+STRING(13b)+STRING(10b)
   n=STRTRIM(self.sino_datei_name,2)&erg=erg+'sino_datei_name: '+(n EQ '' ? snull : n)+STRING(13b)+STRING(10b)
   n=STRTRIM(self.sino_datei_name_err,2)&erg=erg+'sino_datei_name_err: '+(n EQ '' ? snull : n)+STRING(13b)+STRING(10b)
   n=STRTRIM(self.center_offset,2)&erg=erg+'center_offset: '+(n EQ '' ? snull : n)+STRING(13b)+STRING(10b)
   n=STRTRIM(self.startangle,2)&erg=erg+'startangle: '+(n EQ '' ? snull : n)+STRING(13b)+STRING(10b)
   n=STRTRIM(self.recontype,2)&erg=erg+'recontype: '+(n EQ '' ? snull : n)+STRING(13b)+STRING(10b)
   erg=erg+'filter: '+STRTRIM(self.forder,2)+' '+STRTRIM(self.ffrequenz,2)+STRING(13b)+STRING(10b)
   anzahl=0
   e=self.slice_eintrag
   WHILE (e NE PTR_NEW()) DO BEGIN
      anzahl++
      e=(*e).next
   ENDWHILE
   n=STRTRIM(anzahl,2)&erg=erg+'anzahl_slice_eintrag: '+(n EQ '' ? snull : n)+STRING(13b)+STRING(10b)
   e=self.slice_eintrag
   WHILE (e NE PTR_NEW()) DO BEGIN
      n=STRTRIM((*e).slice_rechner_name,2)&erg=erg+'s: '+(n EQ '' ? snull : n)+' '
      n=STRTRIM((*e).slice_datei_name,2)&erg=erg+(n EQ '' ? snull : n)+' '
      n=STRTRIM((*e).slice_datei_name_err,2)&erg=erg+(n EQ '' ? snull : n)+' '+STRTRIM((*e).typ,2)+' '
     erg=erg+STRTRIM((*e).sc_min,2)+' '+STRTRIM((*e).sc_max,2)+' '+STRTRIM((*e).xmin,2)+' '+STRTRIM((*e).xmax,2)+' '
      erg=erg+STRTRIM((*e).ymin,2)+' '+STRTRIM((*e).ymax,2)+' '+STRING(13b)+STRING(10b)
      e=(*e).next
   ENDWHILE
   n=STRTRIM(self.nachricht_rechner,2)&erg=erg+'nachricht_rechner: '+(n EQ '' ? snull : n)+STRING(13b)+STRING(10b)
   n=STRTRIM(self.nachricht_id,2)&erg=erg+'nachricht_id: '+(n EQ '' ? snull : n)+STRING(13b)+STRING(10b)
   n=STRTRIM(self.status,2)&erg=erg+'status: '+(n EQ '' ? snull : n)+STRING(13b)+STRING(10b)
   n=STRTRIM(self.startauftrag,2)&erg=erg+'startauftrag: '+(n EQ '' ? snull : n)+STRING(13b)+STRING(10b)
   n=STRTRIM(self.startrecon,2)&erg=erg+'startrecon: '+(n EQ '' ? snull : n)+STRING(13b)+STRING(10b)
   n=STRTRIM(self.endrecon,2)&erg=erg+'endrecon: '+(n EQ '' ? snull : n)+STRING(13b)+STRING(10b)
   n=STRTRIM(self.endauftrag,2)&erg=erg+'endauftrag: '+(n EQ '' ? snull : n)+STRING(13b)+STRING(10b)
   n=STRTRIM(self.meldung,2)&erg=erg+'meldung: '+(n EQ '' ? snull : n)+STRING(13b)+STRING(10b)
   n=STRTRIM(self.priority,2)&erg=erg+'priority: '+(n EQ '' ? snull : n)+STRING(13b)+STRING(10b)
RETURN, erg
END
