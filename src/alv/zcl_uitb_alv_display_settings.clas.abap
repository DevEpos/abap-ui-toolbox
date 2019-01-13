class ZCL_UITB_ALV_DISPLAY_SETTINGS definition
  public
  inheriting from ZCL_UITB_ALV_METADATA
  final
  create public

  global friends ZCL_UITB_ALV_METADATA_UTIL .

public section.

  methods GET_TITLE
    returning
      value(RESULT) type LVC_TITLE .
  methods SET_TITLE
    importing
      !VALUE type LVC_TITLE .
  methods IS_ROW_MOVE_ALLOWED
    returning
      value(RESULT) type ABAP_BOOL .
  methods SET_ROW_MOVE_ALLOWED
    importing
      !VALUE type ABAP_BOOL default ABAP_TRUE .
  methods HAS_ROW_MARKS
    returning
      value(RESULT) type ABAP_BOOL .
  methods SET_ROW_MARKS
    importing
      !VALUE type ABAP_BOOL default ABAP_TRUE .
  methods IS_ROW_INSERTION_POSSIBLE
    returning
      value(RESULT) type ABAP_BOOL .
  methods SET_ROW_INSERTIONS
    importing
      !VALUE type ABAP_BOOL .
  methods IS_EDITABLE
    returning
      value(RESULT) type ABAP_BOOL .
  methods SET_EDITABLE
    importing
      !VALUE type ABAP_BOOL default ABAP_TRUE .
  methods HIDE_TOOLBAR
    importing
      !VALUE type ABAP_BOOL default ABAP_TRUE .
  methods IS_TOOLBAR_HIDDEN
    returning
      value(RESULT) type ABAP_BOOL .
  methods IS_STRIPED
    returning
      value(RESULT) type ABAP_BOOL .
  methods SET_STRIPED
    importing
      !VALUE type ABAP_BOOL default ABAP_TRUE .
  methods IS_MERGED
    returning
      value(RESULT) type ABAP_BOOL .
  methods SET_MERGED
    importing
      !VALUE type ABAP_BOOL default ABAP_TRUE .
  methods IS_SMALL_TITLE
    returning
      value(RESULT) type ABAP_BOOL .
  methods SET_SMALL_TITLE
    importing
      !VALUE type ABAP_BOOL default ABAP_TRUE .
  methods GET_DRAG_N_DROP
    returning
      value(RESULT) type LVC_S_DD01 .
  methods SET_DRAG_N_DROP
    importing
      !VALUE type LVC_S_DD01 .
  PROTECTED SECTION.
private section.

  data MF_NO_ROW_INSERTIONS type ABAP_BOOL .
  data MF_EDITABLE type ABAP_BOOL .
  data MF_HIDE_TOOLBAR type ABAP_BOOL .
  data MS_DND type LVC_S_DD01 .
  data MF_STRIPED type ABAP_BOOL .
  data MF_NO_MERGING type ABAP_BOOL .
  data MF_NO_ROW_MARKS type ABAP_BOOL .
  data MF_NO_ROW_MOVES type ABAP_BOOL .
  data MV_TITLE type LVC_TITLE .
  data MF_SMALLTITLE type ABAP_BOOL .
ENDCLASS.



CLASS ZCL_UITB_ALV_DISPLAY_SETTINGS IMPLEMENTATION.


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
  ENDMETHOD.


  METHOD set_title.
    mv_title = value.

    set_setter_changed( iv_method = 'SET_TITLE' ).
  ENDMETHOD.
ENDCLASS.
