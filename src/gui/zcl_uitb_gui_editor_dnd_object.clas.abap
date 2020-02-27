"! <p class="shorttext synchronized" lang="en">Drag-n-Drop Object for Editor</p>
CLASS zcl_uitb_gui_editor_dnd_object DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Creates new dnd object for editor</p>
    "!
    METHODS constructor
      IMPORTING
        iv_text          TYPE string
        if_is_long_text  TYPE abap_bool OPTIONAL
        if_adjust_spaces TYPE abap_bool OPTIONAL.
    "! <p class="shorttext synchronized" lang="en">Retrieve stored text</p>
    "!
    METHODS get_text
      IMPORTING
        iv_line_position TYPE i OPTIONAL
      RETURNING
        VALUE(result)    TYPE string.
    "! <p class="shorttext synchronized" lang="en">Is this a long text?</p>
    "!
    METHODS is_long_text
      RETURNING
        VALUE(result) TYPE abap_bool.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_text TYPE string.
    DATA mf_is_long_text TYPE abap_bool.
    DATA mf_adjust_spaces TYPE abap_bool.
ENDCLASS.



CLASS zcl_uitb_gui_editor_dnd_object IMPLEMENTATION.


  METHOD constructor.
    mv_text = iv_text.
    mf_is_long_text = if_is_long_text.
    mf_adjust_spaces = if_adjust_spaces.
  ENDMETHOD.


  METHOD get_text.
    DATA: lt_text TYPE string_table.

    IF mf_is_long_text = abap_true AND
       mf_adjust_spaces = abap_true.

      SPLIT mv_text AT cl_abap_char_utilities=>cr_lf INTO TABLE lt_text.
      LOOP AT lt_text ASSIGNING FIELD-SYMBOL(<lv_line>) FROM 2.
        CONDENSE <lv_line>.
        IF iv_line_position > 0.
          DO iv_line_position - 1 TIMES.
            <lv_line> = | { <lv_line> }|.
          ENDDO.
        ENDIF.
      ENDLOOP.
      CONCATENATE LINES OF lt_text INTO result SEPARATED BY cl_abap_char_utilities=>cr_lf.
    ELSE.
      result = mv_text.
    ENDIF.
  ENDMETHOD.


  METHOD is_long_text.
    result = mf_is_long_text.
  ENDMETHOD.

ENDCLASS.
