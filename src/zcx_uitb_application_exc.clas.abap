CLASS zcx_uitb_application_exc DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES:
      if_t100_message,
      zif_uitb_exception_message.

    ALIASES:
      print_message FOR zif_uitb_exception_message~print.

    CONSTANTS:
      BEGIN OF general_error,
        msgid TYPE symsgid VALUE 'ZUITB_EXCEPTION',
        msgno TYPE symsgno VALUE '000',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE 'MSGV4',
      END OF general_error.

    DATA:
      msgv1 TYPE sy-msgv1,
      msgv2 TYPE sy-msgv2,
      msgv3 TYPE sy-msgv3,
      msgv4 TYPE sy-msgv4.

    METHODS:
      constructor
        IMPORTING
          text     TYPE string OPTIONAL
          textid   LIKE if_t100_message=>t100key OPTIONAL
          previous LIKE previous OPTIONAL
          msgv1    TYPE sy-msgv1 OPTIONAL
          msgv2    TYPE sy-msgv2 OPTIONAL
          msgv3    TYPE sy-msgv3 OPTIONAL
          msgv4    TYPE sy-msgv4 OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_uitb_application_exc IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    DATA: lf_fill_t100_from_sy TYPE abap_bool.

    super->constructor( previous = previous ).

    CLEAR me->textid.

    IF text IS NOT INITIAL.
      lf_fill_t100_from_sy = abap_true.
      zcl_dutils_message_util=>split_string_to_symsg( text ).
    ELSEIF textid IS NOT INITIAL.
      if_t100_message~t100key = textid.
      me->msgv1 = msgv1.
      me->msgv2 = msgv2.
      me->msgv3 = msgv3.
      me->msgv4 = msgv4.
    ELSEIF sy-msgid IS NOT INITIAL.
      lf_fill_t100_from_sy = abap_true.
    ELSE.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ENDIF.

    IF lf_fill_t100_from_sy = abap_true.
      me->msgv1 = sy-msgv1.
      me->msgv2 = sy-msgv2.
      me->msgv3 = sy-msgv3.
      me->msgv4 = sy-msgv4.
      if_t100_message~t100key = VALUE #(
        msgid = sy-msgid
        msgno = sy-msgno
        attr1 = 'MSGV1'
        attr2 = 'MSGV2'
        attr3 = 'MSGV3'
        attr4 = 'MSGV4' ).
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
      iv_msgv4       = msgv4 ).
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
      iv_msgv4        = msgv4 ).
  ENDMETHOD.

ENDCLASS.
