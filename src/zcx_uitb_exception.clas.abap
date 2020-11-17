CLASS zcx_uitb_exception DEFINITION
  PUBLIC
  INHERITING FROM zcx_uitb_nc_exception
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF general_error,
        msgid TYPE symsgid VALUE 'ZUITB_EXCEPTION',
        msgno TYPE symsgno VALUE '000',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE 'MSGV4',
      END OF general_error .
    CONSTANTS:
      BEGIN OF cancelled_by_user,
        msgid TYPE symsgid VALUE 'ZUITB_EXCEPTION',
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF cancelled_by_user .

    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL
        !msgv1    TYPE sy-msgv1 OPTIONAL
        !msgv2    TYPE sy-msgv2 OPTIONAL
        !msgv3    TYPE sy-msgv3 OPTIONAL
        !msgv4    TYPE sy-msgv4 OPTIONAL .

    CLASS-METHODS raise_from_sy
      IMPORTING
        !iv_parameter TYPE dynfnam OPTIONAL
        !iv_line      TYPE i OPTIONAL .
    CLASS-METHODS raise_with_text
      IMPORTING
        !iv_text      TYPE string
        !iv_parameter TYPE dynfnam OPTIONAL
        !iv_line      TYPE i OPTIONAL .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_uitb_exception IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous
        msgv1    = msgv1
        msgv2    = msgv2
        msgv3    = msgv3
        msgv4    = msgv4.
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.

  METHOD raise_from_sy.
    RAISE EXCEPTION TYPE zcx_uitb_exception
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
        msgv4          = sy-msgv4.
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

    RAISE EXCEPTION TYPE zcx_uitb_exception
      EXPORTING
        textid         = general_error
        msgv1          = lv_msgv1
        msgv2          = lv_msgv2
        msgv3          = lv_msgv3
        msgv4          = lv_msgv4.
  ENDMETHOD.
ENDCLASS.
