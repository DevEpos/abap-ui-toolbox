CLASS ZCX_UITB_alv_error DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_message .

    CONSTANTS:
      BEGIN OF ZCX_UITB_alv_error,
        msgid TYPE symsgid VALUE 'ZUITB_EXCEPTION',
        msgno TYPE symsgno VALUE '003',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF ZCX_UITB_alv_error .
    DATA msgv1 TYPE sy-msgv1 .
    DATA msgv2 TYPE sy-msgv2 .
    DATA msgv3 TYPE sy-msgv3 .
    DATA msgv4 TYPE sy-msgv4 .

    METHODS show_message
      IMPORTING
        iv_type         TYPE sy-msgty DEFAULT 'S'
        iv_display_like TYPE sy-msgty DEFAULT 'E'.

    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL
        msgv1     TYPE sy-msgv1 OPTIONAL
        msgv2     TYPE sy-msgv2 OPTIONAL
        msgv3     TYPE sy-msgv3 OPTIONAL
        msgv4     TYPE sy-msgv4 OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCX_UITB_ALV_ERROR IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.

    me->msgv1 = msgv1.
    me->msgv2 = msgv2.
    me->msgv3 = msgv3.
    me->msgv4 = msgv4.

    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.


  METHOD show_message.
    MESSAGE ID if_t100_message~t100key-msgid
            TYPE iv_type
            NUMBER if_t100_message~t100key-msgno
            WITH msgv1
                 msgv2
                 msgv3
                 msgv4
            DISPLAY LIKE iv_display_like.
  ENDMETHOD.
ENDCLASS.
