"! <p class="shorttext synchronized" lang="en">Generic Screen with optional GUI Toolbar</p>
CLASS zcl_uitb_gui_screen DEFINITION
  PUBLIC
  INHERITING FROM zcl_uitb_gui_screen_base
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        iv_title TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_uitb_gui_screen IMPLEMENTATION.

  METHOD constructor.

    super->constructor(
        iv_title = iv_title
        iv_lifetime = cl_gui_control=>lifetime_dynpro
    ).

  ENDMETHOD.
ENDCLASS.
