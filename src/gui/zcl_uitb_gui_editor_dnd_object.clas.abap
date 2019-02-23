"! <p class="shorttext synchronized" lang="en">Drag-n-Drop Object for Editor</p>
CLASS zcl_uitb_gui_editor_dnd_object DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Creates new dnd object for editor</p>
    "!
    METHODS constructor
      IMPORTING
        iv_text         TYPE string
        if_is_long_text TYPE abap_bool OPTIONAL.
    "! <p class="shorttext synchronized" lang="en">Retrieve stored text</p>
    "!
    METHODS get_text
      RETURNING
        VALUE(result) TYPE string.
    "! <p class="shorttext synchronized" lang="en">Is this a long text?</p>
    "!
    METHODS is_long_text
      RETURNING
        VALUE(result) TYPE abap_bool.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_text TYPE string.
    DATA mf_is_long_text TYPE abap_bool.
ENDCLASS.



CLASS zcl_uitb_gui_editor_dnd_object IMPLEMENTATION.


  METHOD constructor.
    mv_text = iv_text.
    mf_is_long_text = if_is_long_text.
  ENDMETHOD.


  METHOD get_text.
    result = mv_text.
  ENDMETHOD.


  METHOD is_long_text.
    result = mf_is_long_text.
  ENDMETHOD.

ENDCLASS.
