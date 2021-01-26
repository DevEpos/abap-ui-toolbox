CLASS zcx_uitb_application_exc DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_t100_message .
    INTERFACES zif_uitb_exception_message.

    ALIASES print_message FOR zif_uitb_exception_message~print.

    DATA msgv1 TYPE sy-msgv1 .
    DATA msgv2 TYPE sy-msgv2 .
    DATA msgv3 TYPE sy-msgv3 .
    DATA msgv4 TYPE sy-msgv4 .

    CONSTANTS:
      BEGIN OF general_error,
        msgid TYPE symsgid VALUE 'ZUITB_EXCEPTION',
        msgno TYPE symsgno VALUE '000',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE 'MSGV4',
      END OF general_error .
    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL
        !msgv1    TYPE sy-msgv1 OPTIONAL
        !msgv2    TYPE sy-msgv2 OPTIONAL
        !msgv3    TYPE sy-msgv3 OPTIONAL
        !msgv4    TYPE sy-msgv4 OPTIONAL .

    CLASS-METHODS raise_appl_err_sy
      RAISING
        zcx_uitb_application_exc.
    CLASS-METHODS raise_appl_error
      IMPORTING
        !iv_text    TYPE string
        ix_previous TYPE REF TO cx_root OPTIONAL
      RAISING
        zcx_uitb_application_exc.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_uitb_application_exc IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = previous ).
    me->msgv1 = msgv1 .
    me->msgv2 = msgv2 .
    me->msgv3 = msgv3 .
    me->msgv4 = msgv4 .
    CLEAR me->textid.

    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.


  METHOD zif_uitb_exception_message~get_message.
    result = zcl_uitb_appl_util=>print_exc_message(
        is_textid      = if_t100_message~t100key
        if_to_screen   = abap_false
        ir_previous    = previous
        ir_exc_message = me
        iv_msgv1       = msgv1
        iv_msgv2       = msgv2
        iv_msgv3       = msgv3
        iv_msgv4       = msgv4
    ).
  ENDMETHOD.


  METHOD zif_uitb_exception_message~print.
    rv_message = zcl_uitb_appl_util=>print_exc_message(
        is_textid       = if_t100_message~t100key
        iv_display_type = iv_display_type
        if_to_screen    = if_to_screen
        iv_message_type = iv_msg_type
        ir_previous     = previous
        ir_exc_message  = me
        iv_msgv1        = msgv1
        iv_msgv2        = msgv2
        iv_msgv3        = msgv3
        iv_msgv4        = msgv4
    ).
  ENDMETHOD.

  METHOD raise_appl_err_sy.
    RAISE EXCEPTION TYPE zcx_uitb_application_exc
      EXPORTING
        textid = VALUE scx_t100key(
           msgid = sy-msgid
           msgno = sy-msgno
           attr1 = 'ATTR1'
           attr2 = 'ATTR2'
           attr3 = 'ATTR3'
           attr4 = 'ATTR4' )
        msgv1  = sy-msgv1
        msgv2  = sy-msgv2
        msgv3  = sy-msgv3
        msgv4  = sy-msgv4.
  ENDMETHOD.


  METHOD raise_appl_error.
    zcl_uitb_appl_util=>split_string_for_message(
      EXPORTING
        iv_string = iv_text
      IMPORTING
        ev_msgv1  = DATA(lv_msgv1)
        ev_msgv2  = DATA(lv_msgv2)
        ev_msgv3  = DATA(lv_msgv3)
        ev_msgv4  = DATA(lv_msgv4)
    ).

    RAISE EXCEPTION TYPE zcx_uitb_application_exc
      EXPORTING
        textid   = general_error
        previous = ix_previous
        msgv1    = lv_msgv1
        msgv2    = lv_msgv2
        msgv3    = lv_msgv3
        msgv4    = lv_msgv4.
  ENDMETHOD.
ENDCLASS.
