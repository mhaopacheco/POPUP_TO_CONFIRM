*&---------------------------------------------------------------------*
*& Report Y_TEST_002
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT y_test_002.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME.

PARAMETERS: p_werks0 LIKE marc-werks DEFAULT '7530',
            p_vkorg0 LIKE mvke-vkorg DEFAULT '7530',
            p_disnso LIKE mvke-vtweg DEFAULT '01',
            p_mbrsh  LIKE mara-mbrsh DEFAULT 'A'.
SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME.
PARAMETERS: p_matnr LIKE mara-matnr.
PARAMETERS: p_matcfg LIKE mara-matnr DEFAULT 'z750-cfgprod-sales'.
SELECTION-SCREEN: END OF BLOCK b2.

START-OF-SELECTION.

  DATA: gt_marc TYPE TABLE OF marc.

  DATA: gt_mara TYPE TABLE OF mara.

  DATA : str_clientdata          LIKE bapi_mara_ga,
         str_plantdata           LIKE bapi_marc_ga,
         str_forecastparameters  LIKE bapi_mpop_ga,
         str_planningdata        LIKE bapi_mpgd_ga,
         str_storagelocationdata LIKE bapi_mard_ga,
         str_valuationdata       LIKE bapi_mbew_ga,
         str_salesdata           LIKE bapi_mvke_ga,
         str_warehousenumberdata LIKE bapi_mlgn_ga,
         str_storagetypedata     LIKE bapi_mlgt_ga,
         str_prtdata             LIKE bapi_mfhm_ga,
         str_lifovaluationdata   LIKE bapi_myms_ga,
         tab_get_all_r           TYPE TABLE OF bapireturn.

  DATA: BEGIN OF tab_head.
      INCLUDE STRUCTURE bapimathead.
  DATA: END OF tab_head.

  DATA: BEGIN OF tab_plantdata.
      INCLUDE STRUCTURE bapi_marc.
  DATA: END OF tab_plantdata.

  DATA: BEGIN OF tab_plantdatax.
      INCLUDE STRUCTURE bapi_marcx.
  DATA: END OF tab_plantdatax.

  DATA: BEGIN OF tab_client.
      INCLUDE STRUCTURE bapi_mara.
  DATA: END OF tab_client.

  DATA: BEGIN OF tab_clientx.
      INCLUDE STRUCTURE bapi_marax.
  DATA: END OF tab_clientx.

  DATA: BEGIN OF tab_salesdata.
      INCLUDE STRUCTURE bapi_mvke.
  DATA: END OF tab_salesdata.

  DATA: BEGIN OF tab_salesdatax.
      INCLUDE STRUCTURE bapi_mvkex.
  DATA: END OF tab_salesdatax.

  DATA : BEGIN OF tab_return.
      INCLUDE STRUCTURE bapiret2 .
  DATA : END OF tab_return.

  DATA : tab_return_msgs TYPE TABLE OF bapi_matreturn2 .

  DATA: tab_forecast   LIKE bapi_mpop,
        tab_forecastx  LIKE bapi_mpopx,
        tab_plangdata  LIKE bapi_mpgd,
        tab_plangdatax LIKE bapi_mpgdx,
        tab_stlocdata  LIKE bapi_mard,
        tab_stlocdatax LIKE bapi_mardx,
        tab_valdata    LIKE bapi_mbew,
        tab_valdatax   LIKE bapi_mbewx,
        tab_warehouse  LIKE bapi_mlgn,
        tab_warehousex LIKE bapi_mlgnx,
        tab_stortyped  LIKE bapi_mlgt,
        tab_stortypedx LIKE bapi_mlgtx.

  DATA gt_makt TYPE TABLE OF bapi_makt.

  DATA: gv_len      TYPE i,
        view,
        counter     TYPE sy-index,
        gv_i        TYPE i,
        gv_msg(200).

  FIELD-SYMBOLS:
    <fs_marc>  TYPE marc,
    <f1>       TYPE any,
    <f2>       TYPE any,
    <fms>      TYPE bapi_matreturn2,
    <fstruct>  TYPE any,
    <fstructx> TYPE any.

  CALL FUNCTION 'BAPI_MATERIAL_GET_ALL'
    EXPORTING
      material            = p_matcfg
      comp_code           = '2000'
      val_area            = '1000'
**      val_type             =
      plant               = p_werks0
*     stge_loc            =
      salesorg            = p_vkorg0
      distr_chan          = p_disnso
*     whsenumber          =
*     stge_type           =
*     lifo_valuation_level =
    IMPORTING
      clientdata          = str_clientdata
      plantdata           = str_plantdata
      forecastparameters  = str_forecastparameters
      planningdata        = str_planningdata
      storagelocationdata = str_storagelocationdata
      valuationdata       = str_valuationdata
      warehousenumberdata = str_warehousenumberdata
      salesdata           = str_salesdata
      storagetypedata     = str_storagetypedata
      prtdata             = str_prtdata
      lifovaluationdata   = str_lifovaluationdata
    TABLES
*     materialdescription =
*     unitsofmeasure      =
*     internationalartnos =
*     materiallongtext    =
*     taxclassifications  =
*     extensionout        =
      return              = tab_get_all_r.



* material views update —

  SELECT * FROM marc INTO TABLE gt_marc
    WHERE matnr = p_matcfg
    AND werks = p_werks0.

  IF sy-subrc = 0.

    READ TABLE gt_marc ASSIGNING <fs_marc> INDEX 1.

    IF sy-subrc = 0.

      counter = 0. CLEAR view.
      gv_i = strlen( <fs_marc>-pstat ).

      DO gv_i TIMES.

        view = <fs_marc>-pstat+counter(sy-index).
        IF view IS INITIAL.
          EXIT.
        ENDIF.
        counter = counter + 1.

        CASE view.
          WHEN 'k'.
            tab_head-basic_view = view.
          WHEN 'v'.
            tab_head-sales_view = view.
          WHEN 'e'.
            tab_head-purchase_view = view.
          WHEN 'd' .
            tab_head-mrp_view = view.
          WHEN 'p'.
            tab_head-forecast_view = view.
          WHEN 'a'.
            tab_head-work_sched_view = view.
          WHEN 'f'.
            tab_head-prt_view = view.
          WHEN 'l'.
            tab_head-storage_view = view.
          WHEN 's'.
            tab_head-warehouse_view = view.
          WHEN 'q'.
            tab_head-quality_view = view.
          WHEN 'b'.
            tab_head-account_view = view.
          WHEN 'g'.
            tab_head-cost_view = view.
          WHEN OTHERS.

        ENDCASE.

      ENDDO.

    ENDIF.

  ENDIF.

  tab_head-material = p_matnr.
  tab_head-ind_sector = str_clientdata-ind_sector.
  tab_head-matl_type = str_clientdata-matl_type.
  MOVE-CORRESPONDING str_clientdata TO tab_client.
  MOVE-CORRESPONDING str_plantdata TO tab_plantdata.
  MOVE-CORRESPONDING str_forecastparameters TO tab_forecast.
  MOVE-CORRESPONDING str_planningdata TO tab_plangdata.
  MOVE-CORRESPONDING str_storagelocationdata TO tab_stlocdata.
  MOVE-CORRESPONDING str_valuationdata TO tab_valdata.
  MOVE-CORRESPONDING str_warehousenumberdata TO tab_warehouse.
  MOVE-CORRESPONDING str_salesdata TO tab_salesdata.
  MOVE-CORRESPONDING str_storagetypedata TO tab_stortyped.

* fill the update struktur



  DO .

    CASE sy-index.

      WHEN 1.

        ASSIGN tab_client TO <f1>.

        ASSIGN tab_clientx TO <f2>.

        PERFORM fill_update_struct USING <f1> <f2>.

      WHEN 2.

        ASSIGN tab_plantdata TO <f1>.

        ASSIGN tab_plantdatax TO <f2>.

        PERFORM fill_update_struct USING <f1> <f2>.

      WHEN 3.

        ASSIGN tab_forecast TO <f1>.

        ASSIGN tab_forecastx TO <f2>.

        PERFORM fill_update_struct USING <f1> <f2>.

      WHEN 4.

        ASSIGN tab_plangdata TO <f1>.

        ASSIGN tab_plangdatax TO <f2>.

        PERFORM fill_update_struct USING <f1> <f2>.

      WHEN 5.

        ASSIGN tab_stlocdata TO <f1>.

        ASSIGN tab_stlocdatax TO <f2>.

        PERFORM fill_update_struct USING <f1> <f2>.

      WHEN 6.

        ASSIGN tab_valdata TO <f1>.

        ASSIGN tab_valdatax TO <f2>.

        PERFORM fill_update_struct USING <f1> <f2>.

      WHEN 7.

        ASSIGN tab_warehouse TO <f1>.

        ASSIGN tab_warehousex TO <f2>.

        PERFORM fill_update_struct USING <f1> <f2>.

      WHEN 8.

        ASSIGN tab_salesdata TO <f1>.

        ASSIGN tab_salesdatax TO <f2>.

        PERFORM fill_update_struct USING <f1> <f2>.

      WHEN 9.

        ASSIGN tab_stortyped TO <f1>.

        ASSIGN tab_stortypedx TO <f2>.

        PERFORM fill_update_struct USING <f1> <f2>.

      WHEN OTHERS.

        EXIT.

    ENDCASE.

  ENDDO.

  tab_salesdata-sales_org = p_vkorg0.
  tab_salesdata-distr_chan = p_disnso.
  CLEAR: tab_salesdatax-sales_org,

  tab_salesdatax-distr_chan.
  tab_salesdatax-sales_org = tab_salesdata-sales_org.
  tab_salesdatax-distr_chan = tab_salesdata-distr_chan.
  CLEAR tab_plantdatax-plant.
  tab_plantdatax-plant = tab_plantdata-plant.

  CLEAR tab_valdatax-val_area.
  tab_valdatax-val_area = tab_valdata-val_area.

  CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
    EXPORTING
      headdata             = tab_head
      clientdata           = tab_client
      clientdatax          = tab_clientx
      plantdata            = tab_plantdata
      plantdatax           = tab_plantdatax
      forecastparameters   = tab_forecast
      forecastparametersx  = tab_forecastx
      planningdata         = tab_plangdata
      planningdatax        = tab_plangdatax
      storagelocationdata  = tab_stlocdata
      storagelocationdatax = tab_stlocdatax
      valuationdata        = tab_valdata
      valuationdatax       = tab_valdatax
      warehousenumberdata  = tab_warehouse
      warehousenumberdatax = tab_warehousex
      salesdata            = tab_salesdata
      salesdatax           = tab_salesdatax
      storagetypedata      = tab_stortyped
      storagetypedatax     = tab_stortypedx
*                             *
*     flag_online          = ' '
*                             *
*     flag_cad_call        = ' '
*                             *
*     no_dequeue           = ' '
    IMPORTING
      return               = tab_return
    TABLES
      materialdescription  = gt_makt
*                             *
*     unitsofmeasure       =
*                             *
*     unitsofmeasurex      =
*                             *
*     internationalartnos  =
*                             *
*     materiallongtext     =
*                             *
*     taxclassifications   =
      returnmessages       = tab_return_msgs.

* prtdata =
* prtdatax =
* extensionin =
* extensioninx =

  IF tab_return-type = 's'.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'x'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ENDIF.

  LOOP AT tab_return_msgs ASSIGNING <fms>.
    gv_msg = <fms>.
    WRITE:/ gv_msg.
  ENDLOOP.


FORM fill_update_struct USING p_f1 TYPE any
                              p_f2 TYPE any.

  DO.

    ASSIGN COMPONENT sy-index OF STRUCTURE p_f1 TO <fstruct>.
    IF sy-subrc <> 0. EXIT. ENDIF.
    IF <fstruct> IS NOT INITIAL.
      ASSIGN COMPONENT sy-index OF STRUCTURE p_f2 TO <fstructx>.
      IF sy-subrc = 0.
        <fstructx> = 'x'.
      ENDIF.
    ENDIF.
  ENDDO.

ENDFORM.