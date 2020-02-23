CLASS zcx_uitb_gui_exception DEFINITION
  PUBLIC
  INHERITING FROM zcx_uitb_nc_exception
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Generic Exception message</p>
      BEGIN OF zcx_uitb_gui_exception,
        msgid TYPE symsgid VALUE 'ZUITB_EXCEPTION',
        msgno TYPE symsgno VALUE '011',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_uitb_gui_exception .

    "! <p class="shorttext synchronized" lang="en">Raises GUI exception with system variables</p>
    CLASS-METHODS raise_from_sy.
    "! <p class="shorttext synchronized" lang="en">Creates new Generic GUI Exception</p>
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



CLASS zcx_uitb_gui_exception IMPLEMENTATION.


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
      if_t100_message~t100key = zcx_uitb_gui_exception.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.

  METHOD raise_from_sy.
    RAISE EXCEPTION TYPE zcx_uitb_gui_exception
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
ENDCLASS.
