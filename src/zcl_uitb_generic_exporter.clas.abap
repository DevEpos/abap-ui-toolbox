CLASS zcl_uitb_generic_exporter DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PROTECTED .

  PUBLIC SECTION.

    METHODS export_data .
  PROTECTED SECTION.
    TYPES: x1024 TYPE x LENGTH 1024.
    DATA mt_xml_data TYPE STANDARD TABLE OF x1024.
    DATA mr_source_data_tab TYPE REF TO data.
    DATA mv_default_file_name TYPE string.
    DATA mv_full_export_path TYPE string.

    METHODS create_internal_table_ref ABSTRACT
      RETURNING
        VALUE(rr_table_ref) TYPE REF TO data.
    METHODS build_data_for_export ABSTRACT.
    METHODS constructor
      IMPORTING
        iv_default_file_name TYPE string.
    METHODS write_success_message ABSTRACT.
    METHODS write_error_message ABSTRACT.
  PRIVATE SECTION.
    METHODS create_xml_string.
    METHODS save_xml_to_file.
ENDCLASS.



CLASS ZCL_UITB_GENERIC_EXPORTER IMPLEMENTATION.


  METHOD constructor.
    IF iv_default_file_name IS INITIAL.
      mv_default_file_name = 'Export'.
    ELSE.
      mv_default_file_name = iv_default_file_name.
    ENDIF.
  ENDMETHOD.


  METHOD create_xml_string.
    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.

    ASSIGN mr_source_data_tab->* TO <lt_table>.
    DATA(lo_xml_writer) = cl_sxml_table_writer=>create( ).

    lo_xml_writer->if_sxml_writer~set_option( option = if_sxml_writer=>co_opt_linebreaks value = abap_true ).

    CALL TRANSFORMATION id
                        SOURCE xmldat = <lt_table>
                        RESULT XML lo_xml_writer.

    lo_xml_writer->get_output( IMPORTING output = mt_xml_data ).
  ENDMETHOD.


  METHOD export_data.
    mr_source_data_tab = create_internal_table_ref( ).

    build_data_for_export( ).

    create_xml_string( ).

    save_xml_to_file( ).
  ENDMETHOD.


  METHOD save_xml_to_file.
    DATA:
      lv_filename      TYPE          string,
      lv_path          TYPE          string,
      lv_fullpath      TYPE          string,
      lv_download_path TYPE          string,
      lv_upload_path   TYPE          string,
      lv_useraction    TYPE          i.

    lv_filename = mv_default_file_name.

    cl_gui_frontend_services=>get_upload_download_path(
      CHANGING
        download_path = lv_download_path
        upload_path   = lv_upload_path
      EXCEPTIONS
        OTHERS        = 1
    ).

    IF sy-subrc <> 0.
      lv_download_path = lv_upload_path = 'C:\'.
    ENDIF.

    IF lv_download_path IS INITIAL.
      lv_download_path = 'C:\'.
    ENDIF.

    cl_gui_frontend_services=>file_save_dialog(
      EXPORTING
        default_extension   = 'XML'
        default_file_name   = lv_filename
        initial_directory   = lv_download_path
        prompt_on_overwrite = 'X'
      CHANGING
        filename            = lv_filename
        path                = lv_path
        fullpath            = lv_fullpath
        user_action         = lv_useraction
    ).

    IF lv_useraction = cl_gui_frontend_services=>action_ok.
      cl_gui_frontend_services=>gui_download(
        EXPORTING
          filename = lv_fullpath
          filetype = 'BIN'
        CHANGING
          data_tab = mt_xml_data
        EXCEPTIONS
          OTHERS   = 1
      ).

      IF sy-subrc = 0.
        IF strlen( lv_fullpath ) >= 50.
          lv_fullpath = substring( val = lv_fullpath len = 47 ) && `...`.
        ENDIF.
        mv_full_export_path = lv_fullpath.
        write_success_message( ).
      ELSE.
        write_error_message( ).
      ENDIF.
    ELSE.
      MESSAGE s002(zuitb_exception) DISPLAY LIKE 'E'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
