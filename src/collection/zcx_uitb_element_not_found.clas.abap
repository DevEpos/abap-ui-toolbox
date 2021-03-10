CLASS zcx_uitb_element_not_found DEFINITION
  PUBLIC
  INHERITING FROM cx_dynamic_check
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES if_t100_message.

    CONSTANTS:
      "! Element not found
      BEGIN OF general,
        msgid TYPE symsgid VALUE 'ZUITB_EXCEPTION',
        msgno TYPE symsgno VALUE '012',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF general.

    CONSTANTS:
      "! Row at index does not exist
      BEGIN OF index_access,
        msgid TYPE symsgid VALUE 'ZUITB_EXCEPTION',
        msgno TYPE symsgno VALUE '013',
        attr1 TYPE scx_attrname VALUE 'INDEX',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF index_access.

    CONSTANTS:
      "! Row with condition at index does not exist
      BEGIN OF condition_access,
        msgid TYPE symsgid VALUE 'ZUTIB_EXCEPTION',
        msgno TYPE symsgno VALUE '014',
        attr1 TYPE scx_attrname VALUE 'COND',
        attr2 TYPE scx_attrname VALUE 'INDEX',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF condition_access.

    DATA index TYPE i.
    DATA cond TYPE string.

    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL
        !index    TYPE i OPTIONAL
        !cond     TYPE string OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_uitb_element_not_found IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    IF textid IS INITIAL.
      if_t100_message~t100key = general.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
    me->index = index.
    me->cond = cond.
  ENDMETHOD.
ENDCLASS.
