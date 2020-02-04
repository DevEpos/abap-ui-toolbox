class ZCL_UITB_ALV_LAYOUT definition
  public
  inheriting from ZCL_UITB_ALV_METADATA
  final
  create public

  global friends ZCL_UITB_ALV_METADATA_UTIL .

public section.

  methods CONSTRUCTOR
    importing
      !IR_CONTROLLER type ref to ZIF_UITB_ALV_METADATA_CTRLLER .
  methods GET_CURRENT_LAYOUT
    returning
      value(RESULT) type ZIF_UITB_ALV_TYPES=>TY_ALV_LAYOUT .
  methods GET_DEFAULT_LAYOUT
    returning
      value(RESULT) type SALV_S_LAYOUT .
  methods GET_KEY
    returning
      value(RESULT) type ZIF_UITB_ALV_TYPES=>TY_ALV_LAYOUT_KEY .
  methods SET_KEY
    importing
      !VALUE type ZIF_UITB_ALV_TYPES=>TY_ALV_LAYOUT_KEY .
  methods HAS_DEFAULT
    returning
      value(RESULT) type ABAP_BOOL .
  methods SET_DEFAULT
    importing
      !VALUE type ABAP_BOOL default ABAP_TRUE .
  methods SET_INITIAL_LAYOUT
    importing
      !VALUE type SLIS_VARI .
  methods GET_INITIAL_LAYOUT
    returning
      value(RESULT) type SLIS_VARI .
  methods SET_SAVE_RESTRICTION
    importing
      !VALUE type I default ZIF_UITB_C_ALV_LAYOUT_RESTRICT=>RESTRICT_NONE .
  methods GET_SAVE_RESTRICTION
    returning
      value(RESULT) type I .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA ms_key TYPE zif_uitb_alv_types=>ty_alv_layout_key .
    DATA ms_current_layout TYPE zif_uitb_alv_types=>ty_alv_layout .
    DATA mv_save_restriction TYPE i .
    DATA mf_use_default TYPE abap_bool value abap_false.
    DATA mv_initial_layout TYPE slis_vari .
ENDCLASS.



CLASS ZCL_UITB_ALV_LAYOUT IMPLEMENTATION.


  METHOD constructor.
    super->constructor(
      io_controller = ir_controller
      iv_name       = 'LAYOUT'
    ).
  ENDMETHOD.


  METHOD get_current_layout.
  ENDMETHOD.


  METHOD get_default_layout.
    DATA: ls_layout  TYPE salv_s_layout_info.

    DATA(lv_restrict) = mv_save_restriction.

    IF mv_save_restriction EQ zif_uitb_c_alv_layout_restrict=>restrict_user_specific.
      lv_restrict = zif_uitb_c_alv_layout_restrict=>restrict_none.
    ENDIF.

    result = CORRESPONDING #(
       cl_salv_layout_service=>get_default_layout(
         s_key    = CORRESPONDING #( ms_key )
         restrict = lv_restrict
       )
    ).

  ENDMETHOD.


  METHOD get_initial_layout.
  ENDMETHOD.


  METHOD get_key.
  ENDMETHOD.


  METHOD get_save_restriction.
    result = mv_save_restriction.
  ENDMETHOD.


  METHOD has_default.
    result = mf_use_default.
  ENDMETHOD.


  METHOD set_default.
    mf_use_default = value.
  ENDMETHOD.


  METHOD set_initial_layout.
    mv_initial_layout = value.
  ENDMETHOD.


  METHOD set_key.
    ms_key = value.
  ENDMETHOD.


  METHOD set_save_restriction.
    mv_save_restriction = value.
  ENDMETHOD.
ENDCLASS.
