INTERFACE zif_uitb_c_alv_functions
  PUBLIC .
  CONSTANTS undo TYPE ui_func VALUE cl_gui_alv_grid=>mc_fc_loc_undo.
  CONSTANTS check_data TYPE ui_func VALUE cl_gui_alv_grid=>mc_fc_check.
  CONSTANTS refresh TYPE ui_func VALUE cl_gui_alv_grid=>mc_fc_refresh.
  CONSTANTS local_paste_menu TYPE ui_func VALUE cl_gui_alv_grid=>mc_mb_paste.
  CONSTANTS local_cut TYPE ui_func VALUE cl_gui_alv_grid=>mc_fc_loc_cut.
  CONSTANTS local_copy TYPE ui_func VALUE cl_gui_alv_grid=>mc_fc_loc_copy.
  CONSTANTS local_paste TYPE ui_func VALUE cl_gui_alv_grid=>mc_fc_loc_paste.
  CONSTANTS local_paste_new_row TYPE ui_func VALUE cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
  CONSTANTS local_copy_row TYPE ui_func VALUE cl_gui_alv_grid=>mc_fc_loc_copy_row.
  CONSTANTS local_insert_row TYPE ui_func VALUE cl_gui_alv_grid=>mc_fc_loc_insert_row.
  CONSTANTS local_append_row TYPE ui_func VALUE cl_gui_alv_grid=>mc_fc_loc_append_row.
  CONSTANTS local_delete_row TYPE ui_func VALUE cl_gui_alv_grid=>mc_fc_loc_delete_row.

  CONSTANTS sort_asc TYPE ui_func VALUE cl_gui_alv_grid=>mc_fc_sort_asc. "#EC NOTEXT
  CONSTANTS sort_desc TYPE ui_func VALUE cl_gui_alv_grid=>mc_fc_sort_dsc. "#EC NOTEXT
  CONSTANTS detail TYPE ui_func VALUE cl_gui_alv_grid=>mc_fc_detail. "#EC NOTEXT
  CONSTANTS outline TYPE ui_func VALUE cl_gui_alv_grid=>mc_fc_auf. "#EC NOTEXT
  CONSTANTS filter_menu TYPE ui_func VALUE cl_gui_alv_grid=>mc_mb_filter.
  CONSTANTS filter TYPE ui_func VALUE cl_gui_alv_grid=>mc_fc_filter. "#EC NOTEXT
  CONSTANTS filter_delete TYPE ui_func VALUE cl_gui_alv_grid=>mc_fc_delete_filter. "#EC NOTEXT
  CONSTANTS quickfilter TYPE ui_func VALUE 'QUICKFILT'.
  constants quickfilter_exclude type ui_func value 'QUICKFILT_EXCL'.
  constants quickfilter_menu type ui_func value 'MB_QUICKFILT'.
  CONSTANTS abc_analysis TYPE ui_func VALUE cl_gui_alv_grid=>mc_fc_call_abc. "#EC NOTEXT
  CONSTANTS view_menu TYPE ui_func VALUE cl_gui_alv_grid=>mc_mb_view.
  CONSTANTS view_excel TYPE ui_func VALUE cl_gui_alv_grid=>mc_fc_view_excel. "#EC NOTEXT
  CONSTANTS view_lotus TYPE ui_func VALUE cl_gui_alv_grid=>mc_fc_view_lotus. "#EC NOTEXT
  CONSTANTS view_crystal TYPE ui_func VALUE cl_gui_alv_grid=>mc_fc_view_crystal. "#EC NOTEXT
  CONSTANTS view_grid TYPE ui_func VALUE cl_gui_alv_grid=>mc_fc_view_grid. "#EC NOTEXT
  CONSTANTS print_preview TYPE ui_func VALUE cl_gui_alv_grid=>mc_fc_print_prev. "#EC NOTEXT
  CONSTANTS print TYPE ui_func VALUE cl_gui_alv_grid=>mc_fc_print_back. "#EC NOTEXT
  CONSTANTS layout_menu TYPE ui_func VALUE cl_gui_alv_grid=>mc_mb_variant.
  CONSTANTS layout_change TYPE ui_func VALUE cl_gui_alv_grid=>mc_fc_current_variant. "#EC NOTEXT
  CONSTANTS layout_save TYPE ui_func VALUE cl_gui_alv_grid=>mc_fc_save_variant. "#EC NOTEXT
  CONSTANTS layout_load TYPE ui_func VALUE cl_gui_alv_grid=>mc_fc_load_variant. "#EC NOTEXT
  CONSTANTS layout_maintain TYPE ui_func VALUE cl_gui_alv_grid=>mc_fc_maintain_variant. "#EC NOTEXT
  CONSTANTS subtotals_menu TYPE ui_func VALUE cl_gui_alv_grid=>mc_mb_subtot.
  CONSTANTS sum_mneu TYPE ui_func VALUE cl_gui_alv_grid=>mc_mb_sum.
  CONSTANTS agg_totals TYPE ui_func VALUE cl_gui_alv_grid=>mc_fc_sum. "#EC NOTEXT
  CONSTANTS agg_min TYPE ui_func VALUE cl_gui_alv_grid=>mc_fc_minimum. "#EC NOTEXT
  CONSTANTS agg_max TYPE ui_func VALUE cl_gui_alv_grid=>mc_fc_maximum. "#EC NOTEXT
  CONSTANTS agg_average TYPE ui_func VALUE cl_gui_alv_grid=>mc_fc_average. "#EC NOTEXT
  CONSTANTS agg_count TYPE ui_func VALUE cl_gui_alv_grid=>mc_fc_count. "#EC NOTEXT
  CONSTANTS subtotals TYPE ui_func VALUE cl_gui_alv_grid=>mc_fc_subtot. "#EC NOTEXT
  CONSTANTS graphics TYPE ui_func VALUE cl_gui_alv_grid=>mc_fc_graph. "#EC NOTEXT
  CONSTANTS export_menu TYPE ui_func VALUE cl_gui_alv_grid=>mc_mb_export.
  CONSTANTS export_xml TYPE ui_func VALUE cl_gui_alv_grid=>mc_fc_call_xml_export. "#EC NOTEXT
  CONSTANTS export_html TYPE ui_func VALUE cl_gui_alv_grid=>mc_fc_html. "#EC NOTEXT
  CONSTANTS export_wordprocessor TYPE ui_func VALUE cl_gui_alv_grid=>mc_fc_word_processor. "#EC NOTEXT
  CONSTANTS export_spreadsheet TYPE ui_func VALUE cl_gui_alv_grid=>mc_fc_call_xxl. "#EC NOTEXT
  CONSTANTS export_localfile TYPE ui_func VALUE cl_gui_alv_grid=>mc_fc_pc_file. "#EC NOTEXT
  CONSTANTS export_mail TYPE ui_func VALUE cl_gui_alv_grid=>mc_fc_to_office. "#EC NOTEXT
  CONSTANTS export_send TYPE ui_func VALUE cl_gui_alv_grid=>mc_fc_send. "#EC NOTEXT
  CONSTANTS system_localfile TYPE ui_func VALUE cl_gui_alv_grid=>mc_fc_pc_file. "#EC NOTEXT
  CONSTANTS find TYPE ui_func VALUE cl_gui_alv_grid=>mc_fc_find. "#EC NOTEXT
  CONSTANTS find_more TYPE ui_func VALUE cl_gui_alv_grid=>mc_fc_find_more. "#EC NOTEXT
  CONSTANTS export_url TYPE ui_func VALUE cl_gui_alv_grid=>mc_fc_url_copy_to_clipboard. "#EC NOTEXT
  CONSTANTS export_folder TYPE ui_func VALUE cl_gui_alv_grid=>mc_fc_call_xint. "#EC NOTEXT
  CONSTANTS all_select TYPE ui_func VALUE cl_gui_alv_grid=>mc_fc_select_all. "#EC NOTEXT
  CONSTANTS all_deselect TYPE ui_func VALUE cl_gui_alv_grid=>mc_fc_deselect_all. "#EC NOTEXT
  CONSTANTS compress TYPE ui_func VALUE '&OMP'.             "#EC NOTEXT
  CONSTANTS expand TYPE ui_func VALUE '&XPA'.               "#EC NOTEXT
  CONSTANTS callreport TYPE ui_func VALUE '&EB9'.           "#EC NOTEXT
  CONSTANTS f1 TYPE ui_func VALUE cl_gui_alv_grid=>mc_fc_help. "#EC NOTEXT
  CONSTANTS continue TYPE ui_func VALUE '&ONT'.             "#EC NOTEXT
  CONSTANTS cancel TYPE ui_func VALUE '&AC1'.               "#EC NOTEXT
  CONSTANTS info TYPE ui_func VALUE '&INFO'.                "#EC NOTEXT
  CONSTANTS f2 TYPE ui_func VALUE '&IC1'.                   "#EC NOTEXT
  CONSTANTS column_invisible TYPE ui_func VALUE cl_gui_alv_grid=>mc_fc_col_invisible. "#EC NOTEXT
  CONSTANTS column_optimze TYPE ui_func VALUE cl_gui_alv_grid=>mc_fc_col_optimize. "#EC NOTEXT
  CONSTANTS columns_to_fix TYPE ui_func VALUE cl_gui_alv_grid=>mc_fc_fix_columns. "#EC NOTEXT
  CONSTANTS columns_to_unfix TYPE ui_func VALUE cl_gui_alv_grid=>mc_fc_unfix_columns. "#EC NOTEXT
  CONSTANTS f4 TYPE ui_func VALUE '&F4'.                    "#EC NOTEXT
  CONSTANTS help TYPE ui_func VALUE cl_gui_alv_grid=>mc_fc_help. "#EC NOTEXT
ENDINTERFACE.
