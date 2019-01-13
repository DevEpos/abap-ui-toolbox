class ZCL_UITB_POPUP_CONTAINER definition
  public
  final
  create public .

public section.

  interfaces ZIF_UITB_VIEW .

  methods CONSTRUCTOR
    importing
      !IV_CAPTION type C .
  methods GET_CONTAINER
    returning
      value(RR_CONTAINER) type ref to CL_GUI_CONTAINER .
  methods CLOSE .
  PROTECTED SECTION.
    DATA mr_dialog TYPE REF TO cl_gui_dialogbox_container.
  PRIVATE SECTION.

    METHODS on_close
         FOR EVENT close OF cl_gui_dialogbox_container .
ENDCLASS.



CLASS ZCL_UITB_POPUP_CONTAINER IMPLEMENTATION.


  METHOD close.
    on_close( ).
  ENDMETHOD.


  METHOD constructor.
    mr_dialog = new #( caption = iv_caption ).
    set HANDLER: on_close for mr_dialog.
  ENDMETHOD.


  METHOD get_container.
    rr_container = mr_dialog.
  ENDMETHOD.


  METHOD on_close.
    mr_dialog->set_visible( abap_false ).
    cl_gui_cfw=>flush( ).
    mr_dialog->free( ).
  ENDMETHOD.


  METHOD zif_uitb_view~show.
    IF iv_start_line IS NOT INITIAL.
      mr_dialog->set_top( iv_start_line ).
    ENDIF.
    IF iv_start_column IS NOT INITIAL.
      mr_dialog->set_left( iv_start_column ).
    ENDIF.
    IF iv_end_line IS NOT INITIAL AND iv_start_line IS NOT INITIAL.
      mr_dialog->set_height( iv_end_line - iv_start_line ).
    ENDIF.
    IF iv_end_column IS NOT INITIAL AND iv_start_column IS NOT INITIAL.
      mr_dialog->set_width( iv_end_column - iv_start_column ).
    ENDIF.

    mr_dialog->set_visible( abap_true ).
    cl_gui_cfw=>flush( ).

  ENDMETHOD.
ENDCLASS.
