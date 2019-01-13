class ZCX_UITB_VALIDATION_ERROR definition
  public
  inheriting from ZCX_UITB_NC_EXCEPTION
  create public .

public section.

  constants:
    begin of GENERAL_ERROR,
      msgid type symsgid value 'ZUITB_EXCEPTION',
      msgno type symsgno value '000',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value 'MSGV3',
      attr4 type scx_attrname value 'MSGV4',
    end of GENERAL_ERROR .
  data PARAMETER_NAME type DYNFNAM read-only .
  data LINE type SY-TABIX read-only .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MSGV1 type SY-MSGV1 optional
      !MSGV2 type SY-MSGV2 optional
      !MSGV3 type SY-MSGV3 optional
      !MSGV4 type SY-MSGV4 optional
      !PARAMETER_NAME type DYNFNAM optional
      !LINE type SY-TABIX optional .
  class-methods RAISE_FROM_SY
    importing
      !IV_PARAMETER type DYNFNAM optional
      !IV_LINE type I optional .
  class-methods RAISE_WITH_TEXT
    importing
      !IV_TEXT type STRING
      !IV_PARAMETER type DYNFNAM optional
      !IV_LINE type I optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_UITB_VALIDATION_ERROR IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
MSGV1 = MSGV1
MSGV2 = MSGV2
MSGV3 = MSGV3
MSGV4 = MSGV4
.
me->PARAMETER_NAME = PARAMETER_NAME .
me->LINE = LINE .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.


  METHOD raise_from_sy.
    RAISE EXCEPTION TYPE zcx_uitb_validation_error
      EXPORTING
        textid         = VALUE scx_t100key(
           msgid = sy-msgid
           msgno = sy-msgno
           attr1 = 'ATTR1'
           attr2 = 'ATTR2'
           attr3 = 'ATTR3'
           attr4 = 'ATTR4' )
        msgv1          = sy-msgv1
        msgv2          = sy-msgv2
        msgv3          = sy-msgv3
        msgv4          = sy-msgv4
        parameter_name = iv_parameter
        line           = iv_line.
  ENDMETHOD.


  METHOD raise_with_text.
    zcl_uitb_appl_util=>split_string_for_message(
      EXPORTING
        iv_string = iv_text
      IMPORTING
        ev_msgv1  = DATA(lv_msgv1)
        ev_msgv2  = DATA(lv_msgv2)
        ev_msgv3  = DATA(lv_msgv3)
        ev_msgv4  = DATA(lv_msgv4)
    ).

    RAISE EXCEPTION TYPE zcx_uitb_validation_error
      EXPORTING
        textid         = general_error
        msgv1          = lv_msgv1
        msgv2          = lv_msgv2
        msgv3          = lv_msgv3
        msgv4          = lv_msgv4
        parameter_name = iv_parameter
        line           = iv_line.

  ENDMETHOD.
ENDCLASS.
