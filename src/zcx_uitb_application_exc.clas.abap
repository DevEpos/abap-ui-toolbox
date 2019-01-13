CLASS ZCX_UITB_APPLICATION_EXC DEFINITION
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

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCX_UITB_APPLICATION_EXC IMPLEMENTATION.


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
ENDCLASS.
