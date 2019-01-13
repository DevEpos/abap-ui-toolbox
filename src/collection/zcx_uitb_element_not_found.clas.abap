CLASS ZCX_UITB_element_not_found DEFINITION
  PUBLIC
  INHERITING FROM cx_dynamic_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS ZCX_UITB_element_not_found TYPE sotr_conc VALUE '005056B375121EE793B9D249FCB75A83' ##NO_TEXT.
    CONSTANTS index_access TYPE sotr_conc VALUE '005056B375121EE793B9E2A44C471A8E' ##NO_TEXT.
    CONSTANTS condition_access TYPE sotr_conc VALUE '005056B375121ED7B2D2BDA517A204D2' ##NO_TEXT.
    DATA index TYPE i .
    DATA cond TYPE string .

    METHODS constructor
      IMPORTING
        !textid   LIKE textid OPTIONAL
        !previous LIKE previous OPTIONAL
        !index    TYPE i OPTIONAL
        !cond     TYPE string OPTIONAL .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCX_UITB_element_not_found IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        textid   = textid
        previous = previous.
    IF textid IS INITIAL.
      me->textid = ZCX_UITB_element_not_found .
    ENDIF.
    me->index = index .
    me->cond = cond .
  ENDMETHOD.
ENDCLASS.
