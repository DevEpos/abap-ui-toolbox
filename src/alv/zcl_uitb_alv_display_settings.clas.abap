CLASS zcl_uitb_alv_display_settings DEFINITION
  PUBLIC
  INHERITING FROM zcl_uitb_alv_metadata
  FINAL
  CREATE PUBLIC

  GLOBAL FRIENDS zcl_uitb_alv_metadata_util .

  PUBLIC SECTION.

    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    METHODS constructor
      IMPORTING
        io_controller TYPE REF TO zif_uitb_alv_metadata_ctrller.
    METHODS get_title
      RETURNING
        VALUE(result) TYPE lvc_title .
    METHODS set_title
      IMPORTING
        !value TYPE lvc_title .
    METHODS is_row_move_allowed
      RETURNING
        VALUE(result) TYPE abap_bool .
    METHODS set_row_move_allowed
      IMPORTING
        !value TYPE abap_bool DEFAULT abap_true .
    METHODS has_row_marks
      RETURNING
        VALUE(result) TYPE abap_bool .
    METHODS set_row_marks
      IMPORTING
        !value TYPE abap_bool DEFAULT abap_true .
    METHODS is_row_insertion_possible
      RETURNING
        VALUE(result) TYPE abap_bool .
    METHODS set_row_insertions
      IMPORTING
        !value TYPE abap_bool .
    METHODS is_editable
      RETURNING
        VALUE(result) TYPE abap_bool .
    METHODS set_editable
      IMPORTING
        !value TYPE abap_bool DEFAULT abap_true .
    METHODS hide_toolbar
      IMPORTING
        !value TYPE abap_bool DEFAULT abap_true .
    METHODS is_toolbar_hidden
      RETURNING
        VALUE(result) TYPE abap_bool .
    METHODS is_striped
      RETURNING
        VALUE(result) TYPE abap_bool .
    METHODS set_striped
      IMPORTING
        !value TYPE abap_bool DEFAULT abap_true .
    METHODS is_merged
      RETURNING
        VALUE(result) TYPE abap_bool .
    METHODS set_merged
      IMPORTING
        !value TYPE abap_bool DEFAULT abap_true .
    METHODS is_small_title
      RETURNING
        VALUE(result) TYPE abap_bool .
    METHODS set_small_title
      IMPORTING
        !value TYPE abap_bool DEFAULT abap_true .
    METHODS get_drag_n_drop
      RETURNING
        VALUE(result) TYPE lvc_s_dd01 .
    METHODS set_drag_n_drop
      IMPORTING
        !value TYPE lvc_s_dd01 .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mf_no_row_insertions TYPE abap_bool .
    DATA mf_editable TYPE abap_bool .
    DATA mf_hide_toolbar TYPE abap_bool .
    DATA ms_dnd TYPE lvc_s_dd01 .
    DATA mf_striped TYPE abap_bool .
    DATA mf_no_merging TYPE abap_bool .
    DATA mf_no_row_marks TYPE abap_bool .
    DATA mf_no_row_moves TYPE abap_bool .
    DATA mv_title TYPE lvc_title .
    DATA mf_smalltitle TYPE abap_bool .
ENDCLASS.



CLASS zcl_uitb_alv_display_settings IMPLEMENTATION.

  method constructor.
    super->constructor(
        io_controller = io_controller
        iv_name       = zif_uitb_c_alv_metadata_types=>display_settings
    ).
  ENDMETHOD.

  METHOD get_drag_n_drop.
    result = ms_dnd.
  ENDMETHOD.


  METHOD get_title.
    result = mv_title.
  ENDMETHOD.


  METHOD has_row_marks.
    result = xsdbool( mf_no_row_marks = abap_false ).
  ENDMETHOD.


  METHOD hide_toolbar.
    mf_hide_toolbar = value.
  ENDMETHOD.


  METHOD is_editable.
    result = mf_editable.
  ENDMETHOD.


  METHOD is_merged.
    result = xsdbool( mf_no_merging = abap_false ).
  ENDMETHOD.


  METHOD is_row_insertion_possible.
    result = xsdbool( mf_no_row_insertions = abap_false ).
  ENDMETHOD.


  METHOD is_row_move_allowed.
    result = xsdbool( mf_no_row_moves = abap_false ).
  ENDMETHOD.


  METHOD is_small_title.
    result = mf_smalltitle.
  ENDMETHOD.


  METHOD is_striped.
    result = mf_striped.
  ENDMETHOD.


  METHOD is_toolbar_hidden.
    result = mf_hide_toolbar.
  ENDMETHOD.


  METHOD set_drag_n_drop.
    ms_dnd = value.
  ENDMETHOD.


  METHOD set_editable.
    mf_editable = value.

    CHECK mr_controller IS BOUND.

    mr_controller->set_changed(
        iv_name         = mv_name
        iv_flavour      = zif_uitb_c_alv_chglist_flavor=>setter
        iv_refresh_mode = zif_uitb_c_alv_refresh=>soft
        iv_method       = 'SET_EDITABLE'
        ir_ref          = me
    ).
  ENDMETHOD.


  METHOD set_merged.
    IF value = abap_true.
      mf_no_merging = abap_false.
    ELSE.
      mf_no_merging = abap_true.
    ENDIF.
    set_setter_changed( iv_method = 'SET_MERGED' ).
  ENDMETHOD.


  METHOD set_row_insertions.
    IF value = abap_true.
      mf_no_row_insertions = abap_false.
    ELSE.
      mf_no_row_insertions = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD set_row_marks.
    IF value = abap_true.
      mf_no_row_marks = abap_false.
    ELSE.
      mf_no_row_marks = abap_true.
    ENDIF.

    set_setter_changed( iv_method = 'SET_ROW_MARKS' ).
  ENDMETHOD.


  METHOD set_row_move_allowed.
    IF value = abap_true.
      mf_no_row_moves = abap_false.
    ELSE.
      mf_no_row_moves = abap_true.
    ENDIF.

    set_setter_changed( iv_method = 'SET_ROW_MOVE_ALLOWED' ).
  ENDMETHOD.


  METHOD set_small_title.
    mf_smalltitle = value.

    set_setter_changed( iv_method = 'SET_SMALL_TITLE' ).
  ENDMETHOD.


  METHOD set_striped.
    mf_striped = value.
    set_setter_changed( iv_method = 'SET_STRIPED' ).
  ENDMETHOD.


  METHOD set_title.
    mv_title = value.

    set_setter_changed( iv_method = 'SET_TITLE' ).
  ENDMETHOD.
ENDCLASS.
