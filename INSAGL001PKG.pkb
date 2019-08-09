CREATE OR REPLACE PACKAGE BODY INSUDB."INSAGL001PKG" AS
PROCEDURE INSAGL001
/*--------------------------------------------------------------------------------------*/
/* NOMBRE    : INSAGL001                                                                */
/* OBJETIVO  : REALIZAR LA PREPARACI¿N CUENTAS CORRIENTES DE INTERMEDIARIOS             */
/* PARAMETROS: 1 - INIT_DATE  : FECHA INICIAL                                           */
/*             2 - END_DATE   : FECHA FINAL                                             */
/*             3 - NUSERCODE  : CODIGO DEL USUARIO                                      */
/*                                                                                      */
/* SOURCESAFE INFORMATION:                                                              */
/*     $Author: cmachado $                                                              */
/*     $Date: 2016-06-24 12:22:30 -0300 (Fri, 24 Jun 2016) $                            */
/*     $Revision: 29806 $                                                               */
/*--------------------------------------------------------------------------------------*/
    (DINIT_DATE                     PREMIUM_MO.DSTATDATE%TYPE,
     DEND_DATE                      PREMIUM_MO.DSTATDATE%TYPE,
     NUSERCODE                      PREMIUM_MO.NUSERCODE%TYPE,
     SPARTIALPAYMENTS               PREMIUM_MO.SCERTYPE%TYPE,
     SKEY                           TMP_AGL001.SKEY%TYPE,
     SPRELIMINARY                   PREMIUM_MO.SCERTYPE%TYPE DEFAULT '1',
     NCOUNTRYCODE                   INTEGER DEFAULT 54) AS

    INIT_DATEAUX                    PREMIUM_MO.DCOMPDATE%TYPE;
    IN_END_DATE                     PREMIUM_MO.DCOMPDATE%TYPE;
    END_DATEAUX                     PREMIUM_MO.DCOMPDATE%TYPE;

    NTYPE_TRAN                      TAB_COMPRO.NTYPE_TRAN%TYPE;
    NTYP_ACCO                       TAB_COMPRO.NTYP_ACCO%TYPE;
    SDEBITSIDE                      TAB_COMPRO.SDEBITSIDE%TYPE;
    NTYPE_MOVE                    MOVE_ACC.NTYPE_MOVE%TYPE;
    NTYPE_AMOUNT                    TAB_COMPRO.NTYP_AMOUNT%TYPE;
    NLINE                           TAB_COMPRO.NLINE%TYPE;

    LOANS_INT_ROWID                 ROWID;

    NACCOKEY                        NUMERATOR.NLASTNUMB%TYPE;
    SDEBITSIDE2                     COMM_DET.SIND_CREDEB%TYPE;
    NCOMDETAMOUNT                   MOVE_ACC.NAMOUNT%TYPE;
    NCOMDETAMOUNT_MA                MOVE_ACC.NAMOUNT%TYPE;
    NCOMMISSION                     AGL001_TEMP_TINTERM_TMP.NAMOUNT%TYPE;
    SCLIENT                         INTERMEDIA.SCLIENT%TYPE;
    NPLUSCOB                        INT_FIXVAL.NRATE%TYPE;
    NAMOUNT2                        MOVE_ACC.NCURRENCY%TYPE;
    NAMODEB                         MOVE_ACC.NDEBIT%TYPE;
    NAMOCRED                        MOVE_ACC.NCREDIT%TYPE;
    SDESCRIPT                       TABLE6.SDESCRIPT%TYPE;
    NIDCONSEC                       MOVE_ACC.NIDCONSEC%TYPE;
    DTODAY                          PREMIUM.DCOMPDATE%TYPE;
    NAUXAMOUNT                      LOANS_INT.NBALANLOAN%TYPE;
    SDESCRIPT_MINI                  TABLE401.SDESCRIPT%TYPE;
    NCOMMISSION_FDRA                FINANC_DRA.NCOMMISSION%TYPE;
    NLIFE_SCHE                      INTERMEDIA.NLIFE_SCHE%TYPE;
    NGEN_SCHE                       INTERMEDIA.NGEN_SCHE%TYPE;
    NINTERTYP                       INTERMEDIA.NINTERMED%TYPE;
    PMO_DCOMPDATE                   PREMIUM_MO.DSTATDATE%TYPE;

    NAMOUNT_RET                     MOVE_ACC.NAMOUNT%TYPE;
    NCREDIT_RET                     MOVE_ACC.NCREDIT%TYPE;
    NDEBIT_RET                      MOVE_ACC.NDEBIT%TYPE;

    NSUM                            NUMBER;
    NCOUNT                          INTEGER;
    NAUXNULL                        NUMBER;
    NPERCENT                        NUMBER(18,6);
    SSELECT                         VARCHAR2(3000);
    SCOMMIS_CO                      CHAR(1);
    MOVEME_INI                      NUMBER(1);
    NPLUSOFFICE                     NUMBER(1);
    NPLUSQUALITY                    NUMBER(1);
    NPERIOD_TYP2                    NUMBER(2);
    SINIT_DATE                      CHAR(10);
    SEND_DATE                       CHAR(10);
    SINIT_MONTH                     CHAR(2);
    SEND_MONTH                      CHAR(2);
    NREST                           NUMBER;
    NTOTALIVA                       NUMBER(18,6);
    NRATEIVA                        NUMBER(1);
    NVALUEIVA                       NUMBER(1);
    NSITIVA                         NUMBER(1);
    STOO_SELCNT                     INTEGER;
    C_COMM_DET_S                    INTEGER;
    C_COMM_DET_S_REG                INTEGER;
    NTOTALCOMMISSION                NUMBER;
    NINDEX                          MOVE_ACC.NIDCONSEC%TYPE;
    NPREM_RECEIPT                   PREMIUM_MO.NPREMIUM%TYPE;
    NBILL_ITEM_AUX                  DETAIL_PRE.NBILL_ITEM%TYPE;

    /* VARIABLES PARA EL PRECALCULO DE LOS VALORES DE LAS RETENCIONES DEL INTERMEDIARIO EN MATERIA DE IIBB */
    NPROVINCE_IIBB_INTER            ADDRESS.NPROVINCE%TYPE;
    SCLIENT_IIBB_INTER              CLIENT.SCLIENT%TYPE;
    SIIBBCOND                       CLIENT_TAX_SITUA.SIIBBCOND%TYPE;

    SINSCRIPTION                    CLIENT_TAX_IIBB.SINSCRIPTION%TYPE;
    NRATEEXEMIB                     CLIENT_TAX_IIBB.NRATEEXEMIB%TYPE;
    DEXEMIB_FROM                    CLIENT_TAX_IIBB.DEXEMIB_FROM%TYPE;
    DEXEMIB_TO                      CLIENT_TAX_IIBB.DEXEMIB_TO%TYPE;

    NMATAX                          IBRETENTION.NMATAX%TYPE;
    NREGTAX                         IBRETENTION.NREGTAX%TYPE;
    NNOTREGTAX                      IBRETENTION.NNOTREGTAX%TYPE;
    NINTERSHARE                     IBRETENTION.NINTERSHARE%TYPE;
    NINCREASEPENALTY           IBRETENTION.NINCREASEPENALTY%TYPE DEFAULT NULL;
    NFIXEDPENALTY                 IBRETENTION.NFIXEDPENALTY%TYPE DEFAULT NULL;

    SUM_NPREMIUM                    PREMIUM.NPREMIUM%TYPE;
    SUM_DISCO_EXPR                  DISC_XPREM.NAMOUNT%TYPE;
    NREC_AMOUNT                     PREMIUM.NPREMIUM%TYPE;
    
    SARA NUMBER;

    IIBBAMOUNT                      NUMBER (18,6);  --importe de retenci¿n de ingreso bruto

    NEXCHANGE                       INTERRETENTIONS.NEXCHANGE%TYPE;
    /* FIN DE VARIABLES PARA EL PRECALCULO DE LOS VALORES DE LAS RETENCIONES DEL INTERMEDIARIO EN MATERIA DE IIBB */

    SDEBIT_SIDE_RET                 VARCHAR(1);
    NID_MOVE_ACC                    MOVE_ACC.NIDCONSEC%TYPE;

    STOO_ERROR  INTEGER;
    STOO_ERRMSG VARCHAR2(500);

    DDATE_END_AUX                   PREMIUM_MO.DSTATDATE%TYPE;
    NCOM_AFEC_AUX                   MOVE_ACC.NAMOAFECT%TYPE;
    NCOM_EXEN_AUX                   MOVE_ACC.NAMOEXEN%TYPE;

    NIBRETTYPE                      INTERMEDIA.NIBRETTYPE%TYPE;

    NPERCENT_AUX                 NUMBER(5,2);
    NADJUSTMENTTYPE_AUX             COMMISS_PR_OLD.NADJUSTMENTTYPE%TYPE;

    NCOUNTERROR                     INTEGER;
    NTYPE_AUX                       PREMIUM_MO.NTYPE%TYPE;
--    NTYPE_PREMIUM PREMIUM.NTYPE%TYPE;


NREM_NUMBER integer; 

    CURSOR C_PREMIUM_MO IS
        SELECT TRUNC(SYSDATE)  "DCOMPDATE"      , PMO.NRECEIPT          , PMO.NTYPE    ,
                   PMO.SCERTYPE         , PMO.NDIGIT            , PMO.NTRANSAC ,
                   PMO.NPREMIUM         ,   0 NDRAFT         , PMO.NAMOUNT           ,
                   PMO.NBRANCH  ,     PMO.NPRODUCT         , PMO.NPAYNUMBE,
                   PRE.NPOLICY,   PRE.NCURRENCY,   PRE.NPREMIUML,
                   PRE.NPREMIUM "NPREMIUMAUX", PRE.NTRATYPEI,
                   PRE.NCONTRAT , PRE.NSTATUS_PRE,
                   PRE.NCERTIF, PRE.NTYPE NTYPE_PREMIUM, PRE.SBILLTYPE,
                  PRE.NRECEIPTLINKED, PRE.DEFFECDATE, PM.SBRANCHT
          FROM PREMIUM_MO PMO, PREMIUM PRE, PRODMASTER PM
         WHERE PMO.SCERTYPE = PRE.SCERTYPE
           AND PMO.NRECEIPT = PRE.NRECEIPT
           AND PMO.NBRANCH = PRE.NBRANCH
           AND PMO.NPRODUCT = PRE.NPRODUCT
           AND PMO.NDIGIT = PRE.NDIGIT
           AND PMO.NPAYNUMBE = PRE.NPAYNUMBE
           AND PMO.NRECEIPT  >   0
           AND PMO.NDIGIT     =  0
           AND PMO.SCERTYPE   = '2'
           AND PMO.DSTATDATE >= INSAGL001.DINIT_DATE
           AND PMO.DSTATDATE <= INSAGL001.END_DATEAUX
           AND NVL(PMO.SINTERMEI, '2')  = '2'
           AND EXISTS (SELECT TCP.NTYPE_TRAN
                                 FROM TAB_COMPRO TCP
                               WHERE TCP.NTYPE_TRAN = PMO.NTYPE)
            AND PM.NBRANCH = PRE.NBRANCH
            AND PM.NPRODUCT = PRE.NPRODUCT;
            ----AND PMO.NRECEIPT = 1000309886;

--         UNION
--        SELECT DH.DLEDGERDAT    ,  FPRE.NRECEIPT    , DH.NTYPE      ,
--               '2'              ,  0                , DH.NTRANSAC   ,
--               FPRE.NPREMIUM    ,  DH.SINTERMEI     , DH.NCONTRAT   ,
--               DH.NDRAFT        ,  DH.NAMOUNT       , FPRE.NBRANCH  ,
--               FPRE.NPRODUCT    ,  0
--          FROM DRAFT_HIST DH,
--               FINANC_PRE FPRE
--         WHERE DH.DSTAT_DATE >= INSAGL001.DINIT_DATE
--           AND DH.DSTAT_DATE <= INSAGL001.END_DATEAUX
--           AND (DH.SINTERMEI = '2'
--            OR DH.SINTERMEI IS NULL)
--           AND DH.NCONTRAT   = FPRE.NCONTRAT
--         ORDER BY 2;



    CURSOR C_COMMISS_PR(PR_NRECEIPT PREMIUM.NRECEIPT%TYPE) IS
        SELECT NINTERMED, SUM(NAMOUNT) NAMOUNT,  NSHARE, NRECEIPT,
                     NPROVINCE, SUM(NCOM_AFEC) NCOM_AFEC, SUM(NCOM_EXEN) NCOM_EXEN, NULL NADJUSTMENTTYPE_AUX
        FROM COMMISS_PR
        WHERE NRECEIPT = PR_NRECEIPT
        AND SCERTYPE = '2'
        GROUP BY SCERTYPE,
                        NBRANCH,
                        NPRODUCT,
                        NRECEIPT,
                        NDIGIT,
                        NPAYNUMBE,
                        NINTERMED,
                        NPROVINCE,
                        NSHARE
        UNION
        SELECT NINTERMED, NAMOUNT,    NSHARE, NRECEIPT,
                 NPROVINCE, NCOM_AFEC, NCOM_EXEN , NADJUSTMENTTYPE
          FROM COMMISS_PR_OLD
         WHERE NRECEIPT    = PR_NRECEIPT
           AND SCERTYPE   = '2'
           AND NTRANSAC  = NTYPE_AUX
           AND NADJUSTMENTTYPE = 3;

    CURSOR C_TAB_COMPRO (PR_NGEN_SCHE TAB_COMPRO.NGEN_SCHE%TYPE ,
                                           PR_NLIFE_SCHE TAB_COMPRO.NLIFE_SCHE%TYPE ,
                                           PR_NTYPE_PREMIUM PREMIUM.NTYPE%TYPE,
                                           PR_NTYPE_PREMIUM_MO PREMIUM_MO.NTYPE%TYPE )
         IS
        SELECT T.NTYPE_TRAN ,   T.NTYP_ACCO, T.SDEBITSIDE,
                    T.NTYP_AMOUNT, T.NLINE,          T.NTYPE_MOVE
          FROM TAB_COMPRO T
         WHERE T.NTYP_ACCO IN (1)
             AND T.NTYPE_TRAN = PR_NTYPE_PREMIUM_MO
             AND T.NADJUSTMENTTYPE = NVL(NADJUSTMENTTYPE_AUX, 0)
             AND T.NGEN_SCHE =  PR_NGEN_SCHE
             AND T.NLIFE_SCHE =  PR_NLIFE_SCHE
             AND T.NTYPE         = PR_NTYPE_PREMIUM
         ORDER BY  T.NTYPE_TRAN , T.NTYP_AMOUNT;

    CURSOR C_DAT_LOAN(PR_NRECEIPT PREMIUM.NRECEIPT%TYPE,
                                      PR_NINTERMED INTERMEDIA.NINTERMED%TYPE,
                                      PR_DINIT_DATE                     PREMIUM_MO.DSTATDATE%TYPE,
                                      PR_DEND_DATEAUX                PREMIUM_MO.DCOMPDATE%TYPE)
     IS
        SELECT NLOAN, NAMOUNT
          FROM DAT_LOAN
         WHERE NINTERMED = PR_NINTERMED
           AND NRECEIPT  = PR_NRECEIPT
           AND SINDICAT  = '2'
           AND DLOAN    >= PR_DINIT_DATE
           AND DLOAN    <= PR_DEND_DATEAUX ;

    CURSOR C_LOANS_INT (PR_NINTERMED INTERMEDIA.NINTERMED%TYPE) IS
        SELECT NBALANLOAN , NCURRENCY, NRATE_RET,
                    SSTATLOAN,  ROWID NROW
          FROM LOANS_INT
         WHERE NINTERMED  = PR_NINTERMED
           AND NBALANLOAN > 0
           AND NFOR_PAY   = 1;

    CURSOR C_INTERM_BUD (PR_NINTERMED INTERMEDIA.NINTERMED%TYPE) IS
        SELECT COUNT(NCURRENCY)
          FROM INTERM_BUD
         WHERE NINTERMED = PR_NINTERMED;

    CURSOR C_INTERMEDIA IS
        SELECT CTTT.NCODE        , INTERM_BUD.NREAL_TOTAL, INTERM_BUD.NCURRENCY ,
               INTERM_BUD.NYEAR  , INTERM_BUD.SPERIODTYP , INTERM_BUD.NPERIODNUM,
               INTERM_BUD.NBRANCH, INTERM_BUD.NPRODUCT   , INTERM_BUD.STYPE_INFOR
          FROM AGL001_TEMP_TINTERCODE_TMP CTTT,
               INTERM_BUD INTERM_BUD
         WHERE(INTERM_BUD.NINTERMED = CTTT.NCODE
           AND INTERM_BUD.DNULLDATE IS NULL
            OR INTERM_BUD.DNULLDATE = '')
           AND CTTT.SKEY = INSAGL001.SKEY;


    CURSOR C_DETAIL_PRE (NRECEIPT_AUX PREMIUM.NRECEIPT%TYPE) IS
        SELECT  NPROVINCE,          NPREMIUM,           NCOMMISION
          FROM  DETAIL_PRE
         WHERE  NRECEIPT        =   NRECEIPT_AUX
           AND  STYPE_DETAI     =   '1';

        COUNT_REG INTEGER;

    TYPE  RT_MAN_MOV_ACC IS RECORD
            (NTYP_ACCO       MOVE_ACC.NTYP_ACCO%TYPE,
            STYPE_ACC        MOVE_ACC.STYPE_ACC%TYPE,
            SCLIENT          MOVE_ACC.SCLIENT%TYPE,
            NCURRENCY        MOVE_ACC.NCURRENCY%TYPE,
            DOPERDATE        MOVE_ACC.DOPERDATE%TYPE,
            NIDCONSEC        MOVE_ACC.NIDCONSEC%TYPE,
            NINTERMED        MOVE_ACC.NINTERMED%TYPE,
            NAMOUNT          MOVE_ACC.NAMOUNT%TYPE,
            NDEBIT           MOVE_ACC.NDEBIT%TYPE,
            NCREDIT          MOVE_ACC.NCREDIT%TYPE,
            NINTERTYP        INTERMEDIA.NINTERTYP%TYPE,
            NTYPE_MOVE       MOVE_ACC.NTYPE_MOVE%TYPE,
            NPROVINCE        MOVE_ACC.NPROVINCE%TYPE,
            SCERTYPE         MOVE_ACC.SCERTYPE%TYPE,
            NBRANCH          MOVE_ACC.NBRANCH%TYPE,
            NPRODUCT         MOVE_ACC.NPRODUCT%TYPE,
            NRECEIPT         MOVE_ACC.NRECEIPT%TYPE,
            NPOLICY          MOVE_ACC.NPOLICY%TYPE,
            NDIGIT           MOVE_ACC.NDIGIT%TYPE,
            NPAYNUMBE        MOVE_ACC.NPAYNUMBE%TYPE,
            NEXCHANGE        MOVE_ACC.NEXCHANGE%TYPE,
            SSOCIALRET       CURRACCLINES.SSOCIALRET%TYPE,
            SIIBBRET         CURRACCLINES.SIIBBRET%TYPE,
            SREACHTDC        CURRACCLINES.SREACHTDC%TYPE,
            NVATAFFECTYPE    CURRACCLINES.NVATAFFECTYPE%TYPE,
            SPROFITRET       CURRACCLINES.SPROFITRET%TYPE,
            NACCDIST         CURRACCLINES.NACCDIST%TYPE,
            NAMOAFECT        MOVE_ACC.NAMOAFECT%TYPE,
            NAMOEXEN         MOVE_ACC.NAMOEXEN%TYPE,
            NAMONOCLAS       MOVE_ACC.NAMONOCLAS%TYPE,
            SDESCRIPT           MOVE_ACC.SDESCRIPT%TYPE
            );

    R_MAN_MOV_ACC RT_MAN_MOV_ACC;

    CURSOR C_MAN_MOV_ACC IS
        SELECT MA.NTYP_ACCO    , MA.STYPE_ACC     , MA.SCLIENT  ,
               MA.NCURRENCY    , MA.DOPERDATE     , MA.NIDCONSEC ,
               MA.NINTERMED    , MA.NAMOUNT       , MA.NDEBIT,
               MA.NCREDIT      , I.NINTERTYP      , MA.NTYPE_MOVE,
               MA.NPROVINCE       , MA.SCERTYPE      , MA.NBRANCH,
               MA.NPRODUCT     , MA.NRECEIPT      , MA.NPOLICY    , MA.NDIGIT,
               MA.NPAYNUMBE    , MA.NEXCHANGE      , CAL.SSOCIALRET,
               CAL.SIIBBRET       , CAL.SREACHTDC      , CAL.NVATAFFECTYPE,
               CAL.SPROFITRET  , CAL.NACCDIST,      MA.NAMOAFECT,
               MA.NAMOEXEN     , MA.NAMONOCLAS, MA.SDESCRIPT
          FROM MOVE_ACC MA, CURRACCLINES CAL, INTERMEDIA I
         WHERE MA.SMANUALMOV  = '1'
           AND MA.DOPERDATE   >= INSAGL001.DINIT_DATE
           AND MA.DOPERDATE   <= INSAGL001.END_DATEAUX
           AND MA.SSTATREGT = '1'
           AND MA.NTYP_ACCO = CAL.NTYP_ACCO
           AND MA.NTYPE_MOVE = CAL.NTYPE_MOVE
           AND CAL.DEFFECDATE <= MA.DOPERDATE
           AND (CAL.DNULLDATE IS NULL
                OR  CAL.DNULLDATE  > MA.DOPERDATE)
           AND MA.NINTERMED = I.NINTERMED
           AND CAL.NINTERTYP = I.NINTERTYP
           AND MA.SPROCESS_IND IS NULL
ORDER BY MA.NTYP_ACCO, MA.STYPE_ACC, MA.SCLIENT;

    CURSOR C_CURRACCLINES_ERRS IS
       SELECT 'No existe gu¿a de liquidaci¿n para el movimiento manual ' || TO_CHAR(CAM.NTYPE_MOVE) || ' ' || TRIM(REAGENERALPKG.REASTYPE_MOVE(CAM.NTYPE_MOVE)) || ', tipo de cuenta ' || TO_CHAR(CAM.NTYP_ACCO) || '.' sDescript
           FROM CURRACCMOV CAM, CURRACCLINES CAL
          WHERE CAM.NTYP_ACCO = CAL.NTYP_ACCO(+)
            AND CAM.NTYPE_MOVE = CAL.NTYPE_MOVE(+)
            AND CAL.NTYP_ACCO IS NULL;

   SERRORDESC                      VARCHAR2(400);
   NERRORNUM                       INTEGER;


   DINITDATE                       PREMIUM.DEFFECDATE%TYPE;
   NCOMPANY                        OPT_SYSTEM.NCOMPANY%TYPE;
   NSHARE_COIN                     COINSURAN.NSHARE%TYPE;
   NACTIVITYLEVEL       CLIENT_TAX_IIBB.NACTIVITYLEVEL%TYPE;

--   NEXIST_OFFER NUMBER;

BEGIN

    VTBATCHPKG.STARTBATCH(SKEY);

    BEGIN
        SELECT NCOMPANY
          INTO NCOMPANY
          FROM OPT_SYSTEM;

    EXCEPTION
        WHEN OTHERS THEN
            NCOMPANY := NULL;
    END;

    INIT_DATEAUX        := DINIT_DATE   ;
    IN_END_DATE         := DEND_DATE    ;
    END_DATEAUX         := DEND_DATE + 1;
    NAUXNULL            := NULL;
    DTODAY              :=  SYSDATE;
    NINDEX              := 0;
    NERRORNUM           := 0;
    /* ENUNEZ: SE USARA LA FECHA FINAL DEL PROCESO PARA HACER LAS INSERSIONES EN LAS TABLAS MOVE_ACC, COMM_DET E INTERRETENTION */
    DDATE_END_AUX       := DEND_DATE    ;

    INSNUMERATORSP(24,0,NACCOKEY);

FOR R_CURRACCLINES_ERRS IN C_CURRACCLINES_ERRS LOOP
--CRETRACE2('INSAGL001 1', 984, 'INICIO C_CURRACCLINES_ERRS ' || TO_CHAR(SYSTIMESTAMP,'dd-mm-yyyy hh24:mi:ss.FF'));

        BEGIN
            NERRORNUM := NERRORNUM + 1;
            INSERT INTO TIMETMP.TMP_BATCH_LOG  (SKEY, NMESSSEQ, NMESSLINE, NMESSCOD, SLOG, DCOMPDATE, NUSERCODE)
            VALUES (INSAGL001.SKEY, 1, NERRORNUM, 1, R_CURRACCLINES_ERRS.SDESCRIPT,  SYSDATE, INSAGL001.NUSERCODE);
        END;

--CRETRACE2('INSAGL001 2', 984, 'FIN C_CURRACCLINES_ERRS ' || TO_CHAR(SYSTIMESTAMP,'dd-mm-yyyy hh24:mi:ss.FF'));
END LOOP;

    COMMIT;

    SELECT COUNT(1)
      INTO NCOUNTERROR
      FROM TIMETMP.TMP_BATCH_LOG
     WHERE SKEY = INSAGL001.SKEY
       AND NMESSSEQ = 1
       AND NMESSCOD = 1;

    IF NCOUNTERROR = 0 THEN



FOR R_PREMIUM_MO IN C_PREMIUM_MO LOOP
--CRETRACE2('INSAGL001 3', 984, 'INICIO POR MOVIMIENTO DE RECIBO ' || R_PREMIUM_MO.NRECEIPT || 'hora |' || TO_CHAR(SYSTIMESTAMP,'dd-mm-yyyy hh24:mi:ss.FF'));

 R_PREMIUM_MO.NPREMIUM := ABS(R_PREMIUM_MO.NPREMIUM); /*+ CHECKEAR - SARA*/


BEGIN
             SELECT NRATE
                INTO NPLUSCOB
               FROM INT_FIXVAL
             WHERE NCODE       = 8
               AND DEFFECDATE <= R_PREMIUM_MO.DCOMPDATE
               AND (DNULLDATE IS NULL
                OR DNULLDATE   < R_PREMIUM_MO.DCOMPDATE);

    EXCEPTION
              WHEN NO_DATA_FOUND THEN
                NPLUSCOB := NULL;
  END;

        BEGIN

             NTYPE_AUX := R_PREMIUM_MO.NTRANSAC;

            BEGIN

FOR R_COMMISS_PR IN C_COMMISS_PR(R_PREMIUM_MO.NRECEIPT) LOOP
--CRETRACE2('INSAGL001 4', 984, 'INICIO POR C_COMMISS_PR ' || TO_CHAR(SYSTIMESTAMP,'dd-mm-yyyy hh24:mi:ss.FF'));

nidconsec := 0;

NADJUSTMENTTYPE_AUX := R_COMMISS_PR.NADJUSTMENTTYPE_AUX; /*+ CHECKEAR - SARA */

                BEGIN
                    NPLUSOFFICE :=  0;
                    NPLUSQUALITY :=  0;
                    NPLUSCOB :=  0;

                    BEGIN

    /*+ SE LEE LA TABLA DE INTERMEDIARIOS (INTERMEDIA) PARA OBTENER LA FORMA DE PAGO DE COMISIONES DE VIDA, PARA SER UTILIZADO EN CASO DE QUE EL RECIBO SEA DE VIDA (NLIFE_SCHE) Y LA FORMA DE PAGO +*/
    /*+ DE COMISIONES DE GENERALES (NGEN_SCHE), PARA SER UTILIZADO EN CASO DE QUE EL RECIBO SEA DE GENERALES +*/

                         SELECT SCLIENT,    NLIFE_SCHE,     NGEN_SCHE,
                                NINTERTYP,  NIBRETTYPE
                           INTO SCLIENT,     NLIFE_SCHE,     NGEN_SCHE,
                                NINTERTYP,      NIBRETTYPE
                           FROM INTERMEDIA
                          WHERE NINTERMED = R_COMMISS_PR.NINTERMED;

                    EXCEPTION
                        WHEN NO_DATA_FOUND THEN
                            SCLIENT     := NULL;
                            NLIFE_SCHE  := 0;
                            NGEN_SCHE   := 0;
                            NINTERTYP   := NULL;
                    END;

                    /*+ Si el ramo es de vida, debe buscar la parametrización de generales  y viceversa+*/

                    IF R_PREMIUM_MO.SBRANCHT = '1' THEN
                        NGEN_SCHE   := 0;
                    ELSE
                         NLIFE_SCHE  := 0;
                    END IF;

                    NTOTALIVA :=  0;

    /*+  BASADOS EN EL TIPO DE TRANSACCI¿N OBTENIDA, SE LEE LA TABLA DE GU¿A PARA ACTUALIZACI¿N DE CUENTAS CORRIENTES DE (TAB_COMPRO) PARA DETERMINAR LOS VALORES QUE SE TOMAR¿N EN +*/
    /*+ CUENTA PARA CADA TIPO DE TRANSACCI¿N. ENTONCES, SE GENERA UN MOVIMIENTO DE CR¿DITO O D¿BITO EN LA TABLA DE DETALLE DE MOVIMIENTO DE COMISIONES EN CUENTA CORRIENTE (COMM_DET). +*/
    /*+ LA TABLA PUEDE INCLUIR TRES MONTOS DIFERENTES: +*/
    /*+ COMISI¿N/HONORARIO POR SERVICIO: SE ENCUENTRA EN LA TABLA DE COMISIONES DE UNA FACTURA (COMMISS_PR). +*/

--CRETRACE2('INSAGL001 4.1', 984, 'consulta C_TAB_COMPRO NTYPE_TRAN' || R_PREMIUM_MO.NTYPE || ' NADJUSTMENTTYPE_AUX ' || NVL(R_COMMISS_PR.NADJUSTMENTTYPE_AUX, 0) || ' INSAGL001.NGEN_SCHE ' || INSAGL001.NGEN_SCHE || ' INSAGL001.NLIFE_SCHE ' || INSAGL001.NLIFE_SCHE || ' INSAGL001.NTYPE_PREMIUM ' || R_PREMIUM_MO.NTYPE_PREMIUM);

FOR R_TAB_COMPRO IN C_TAB_COMPRO(NGEN_SCHE,NLIFE_SCHE,R_PREMIUM_MO.NTYPE_PREMIUM, R_PREMIUM_MO.NTYPE) LOOP

--CRETRACE2('INSAGL001 5', 984, 'INICIO POR C_TAB_COMPRO ' || TO_CHAR(SYSTIMESTAMP,'dd-mm-yyyy hh24:mi:ss.FF'));

NTYPE_TRAN := R_TAB_COMPRO.NTYPE_TRAN; /*+ CHECKEAR SARA */
NTYP_ACCO := R_TAB_COMPRO.NTYP_ACCO;
SDEBITSIDE := R_TAB_COMPRO.SDEBITSIDE;
NTYPE_AMOUNT := R_TAB_COMPRO.NTYP_AMOUNT;
NLINE := R_TAB_COMPRO.NLINE;
NTYPE_MOVE := R_TAB_COMPRO.NTYPE_MOVE;

                    BEGIN

                        NCOMDETAMOUNT :=  0;

               BEGIN
                            BEGIN
                                SELECT 1
                                  INTO STOO_SELCNT
                                  FROM DUAL
                                 WHERE NOT EXISTS (SELECT SCLIENT
                                                     FROM CURR_ACC
                                                    WHERE NTYP_ACCO = R_TAB_COMPRO.NTYP_ACCO
                                                      AND STYPE_ACC    = '0'
                                                      AND SCLIENT         = INSAGL001.SCLIENT
                                                      AND NCURRENCY    = R_PREMIUM_MO.NCURRENCY);
                            EXCEPTION
                                WHEN NO_DATA_FOUND THEN
                                    STOO_SELCNT := 0;
                            END;

                            IF STOO_SELCNT != 0 THEN
                                CRECURR_ACC(NTYP_ACCO   => NTYP_ACCO,
                                                        STYPE_ACC   => '0',
                                                        SCLIENT     => SCLIENT,
                                                        NCURRENCY   => R_PREMIUM_MO.NCURRENCY,
                                                        NBALANCE    => 0,
                                                        DEFFECDATE  => R_PREMIUM_MO.DCOMPDATE,
                                                        SSTATREGT   => '1',
                                                        NUSERCODE   => NUSERCODE,
                                                        NLED_COMPAN => NULL,
                                                        SACCOUNT    => '',
                                                        NCOMMIT     => 2);
                            END IF;

    /*+ BASADOS EN EL TIPO DE TRANSACCI¿N OBTENIDA, SE LEE LA TABLA DE GU¿A PARA ACTUALIZACI¿N DE CUENTAS CORRIENTES DE (TAB_COMPRO) PARA DETERMINAR LOS VALORES QUE SE TOMAR¿N EN CUENTA PARA CADA TIPO +*/
    /*+ DE TRANSACCI¿N. ENTONCES, SE GENERA UN MOVIMIENTO DE CR¿DITO O D¿BITO EN LA TABLA DE DETALLE DE MOVIMIENTO DE COMISIONES EN CUENTA CORRIENTE (COMM_DET).+*/

    /*+ TIPO DE MONTO: PRIMA TOTAL  +*/
--CRETRACE2('INSAGL001 5.1', 984, 'INICIO POR C_TAB_COMPRO ' || TO_CHAR(SYSTIMESTAMP,'dd-mm-yyyy hh24:mi:ss.FF'));

                                IF NTYPE_AMOUNT = 1 THEN
                                BEGIN
                                    IF NVL(R_PREMIUM_MO.NPREMIUM, 0) <> 0 THEN
                                        IF R_PREMIUM_MO.NPREMIUM < 0 THEN
                                            R_PREMIUM_MO.NPREMIUM :=  R_PREMIUM_MO.NPREMIUM * (-1);
                                        END IF;

                                        IF R_PREMIUM_MO.NPREMIUM = R_PREMIUM_MO.NPREMIUMAUX THEN
                                            NCOMDETAMOUNT :=  R_PREMIUM_MO.NPREMIUMAUX;
                                        ELSE
                                            NPERCENT :=  R_PREMIUM_MO.NPREMIUM * 100 / R_PREMIUM_MO.NPREMIUMAUX;
                                            NCOMDETAMOUNT :=  NPERCENT * R_PREMIUM_MO.NPREMIUMAUX / 100;
                                        END IF;
                                    END IF;
                                END;

     /*+ TIPO DE MONTO: PRIMA L¿QUIDA  +*/

                                ELSIF NTYPE_AMOUNT = 2 THEN
                                BEGIN
                                    IF NVL(R_PREMIUM_MO.NPREMIUM, 0) <> 0 THEN
                                        R_PREMIUM_MO.NPREMIUM := ABS(R_PREMIUM_MO.NPREMIUM);
                                        IF R_PREMIUM_MO.NPREMIUM = R_PREMIUM_MO.NPREMIUML THEN --SI LA PRIMA DEL MOVIMIENTO DE RECIBO ES IGUAL A LA PRIMA LIQUIDA
                                            NCOMDETAMOUNT :=  R_PREMIUM_MO.NPREMIUML;
                                        ELSE
                                            NPERCENT :=  (R_PREMIUM_MO.NPREMIUM * 100) / R_PREMIUM_MO.NPREMIUML; --EL PORCENTAJE DEL MOVIMIENTO DE PRIMA SOBRE LA PRIMA LIQUIDA
                                            NCOMDETAMOUNT :=  NPERCENT * R_PREMIUM_MO.NPREMIUML / 100; --APLICO EL PORCENTAJE SOBRE LA PRIMA LIQUIDA
                                        END IF;
                                    END IF;
                                END;

    /*+ TIPO DE MONTO: COMISI¿N  +*/

                                ELSIF NTYPE_AMOUNT = 3 THEN
                                        IF NTYPE_TRAN = 40 THEN
                                            BEGIN

                                                SELECT NPERCENTADJ
                                                  INTO NPERCENT_AUX
                                                  FROM COMMISS_PR_OLD
                                                 WHERE SCERTYPE = '2'
                                                   AND NBRANCH = R_PREMIUM_MO.NBRANCH
                                                   AND NPRODUCT = R_PREMIUM_MO.NPRODUCT
                                                   AND NRECEIPT = R_PREMIUM_MO.NRECEIPT
                                                   AND NDIGIT = 0
                                                   AND NPAYNUMBE = 0
                                                   AND NINTERMED = R_COMMISS_PR.NINTERMED
                                                   AND NTRANSAC = R_PREMIUM_MO.NTRANSAC
                                                   AND NADJUSTMENTTYPE = 3;
                                            EXCEPTION
                                                WHEN NO_DATA_FOUND THEN
                                                    NPERCENT_AUX := 0;
                                            END;
                                        END IF;
                                        BEGIN
                                            IF NVL(R_PREMIUM_MO.NPREMIUM, 0) <> 0 THEN
                                            BEGIN

            /*+ SI LA OPCI¿N DE PAGO DE COMISIONES POR PAGOS PARCIALES NO EST¿ SELECCIONADA  (PROYECTO GS) +*/
            /*+ SI EL MOVIMIENTO EN TRATAMIENTO ES DE PAGO O DE REVERSO DE PAGO DE PRIMA +*/
            /*+ SI EL RECIBO SE ENCUENTRA FINANCIADO, SE LIBERA EL MONTO DE COMISI¿N QUE CORRESPONDE A LA CUOTA EN TRATAMIENTO (FINANC_DRA). +*/

                                                IF (INSAGL001.SPARTIALPAYMENTS !=  '1'
                                               AND  R_PREMIUM_MO.NTYPE IN (34, 4) -- NTYPE = 34 - LIQUIDACI¿N DEL COBRO
                                               AND  R_PREMIUM_MO.NSTATUS_PRE = 8) THEN
                                                BEGIN

                                                    BEGIN
                                                        SELECT NCOMMISSION,      NCOM_AFEC,  NCOM_EXEN
                                                          INTO NCOMMISSION_FDRA, R_COMMISS_PR.NCOM_AFEC,  R_COMMISS_PR.NCOM_EXEN
                                                          FROM FINANC_DRA
                                                         WHERE NCONTRAT = R_PREMIUM_MO.NCONTRAT
                                                           AND NDRAFT     = R_PREMIUM_MO.NDRAFT;
                                                    EXCEPTION
                                                        WHEN NO_DATA_FOUND THEN
                                                            NCOMMISSION_FDRA := NULL;
                                                            R_COMMISS_PR.NCOM_AFEC        := NULL;
                                                            R_COMMISS_PR.NCOM_EXEN        := NULL;
                                                    END;

                                                    NCOMDETAMOUNT := NCOMMISSION_FDRA;

                                                END;

            /*+ SI EL MOVIMIENTO EN TRATAMIENTO ES DE PAGO O DE REVERSO DE PAGO DE PRIMA, TOMANDO EN CUENTA LOS PAGOS PARCIALES QUE PUDIERA TENER, SE OBTIENE EL PORCENTAJE PAGADO DE LA DEUDA: +*/
            /*+  % = (IMPORTE DEL PAGO EN TRATAMIENTO/ IMPORTE TOTAL DE DEUDA DEL RECIBO O DE LA CUOTA) * 100 +*/
            /*+ EL IMPORTE DE LA COMISI¿N A LIBERAR SER¿ LA COMISI¿N ASOCIADA AL RECIBO/CUOTA POR EL PORCENTAJE OBTENIDO PREVIAMENTE. +*/

                                                ELSIF (INSAGL001.SPARTIALPAYMENTS =  '1' ) THEN
                                                BEGIN
                                                    --NPERCENT :=  ((NPREMIUM / NPREMIUMAUX) * 100); --PREMIUM_MO.NPREMIUM / PREMIUM.NPREMIUM
                                                    IF NTYPE_TRAN = 40 THEN
                                                        NPERCENT := NPERCENT_AUX;
                                                    ELSE
                                                        NPERCENT :=  ((R_PREMIUM_MO.NAMOUNT / R_PREMIUM_MO.NPREMIUMAUX) * 100);
                                                    END IF;

                                                    NCOMDETAMOUNT := (R_COMMISS_PR.NAMOUNT * NPERCENT / 100);

                                                    IF R_COMMISS_PR.NCOM_EXEN = 0 OR R_COMMISS_PR.NCOM_EXEN IS NULL THEN
                                                        NCOM_EXEN_AUX := 0;
                                                        NCOM_AFEC_AUX := NCOMDETAMOUNT;
                                                    ELSE
                                                        NCOM_EXEN_AUX := NCOMDETAMOUNT;
                                                        NCOM_AFEC_AUX := 0;
                                                    END IF;

                                                END;

                                                ELSE
                                                BEGIN
                                                    R_PREMIUM_MO.NPREMIUM := ABS(R_PREMIUM_MO.NPREMIUM);

                                                    IF R_PREMIUM_MO.NPREMIUM = R_COMMISS_PR.NAMOUNT THEN -- PRIMATOTALRECIBO = MONTODECOMISI¿N
                                                        NCOMDETAMOUNT :=  R_COMMISS_PR.NAMOUNT;
                                                    ELSE
                                                        IF NTYPE_TRAN = 40 THEN
                                                            NPERCENT := NPERCENT_AUX;
                                                        ELSE
                                                            NPERCENT :=  (R_COMMISS_PR.NAMOUNT * 100) / R_PREMIUM_MO.NPREMIUM;
                                                        END IF;
                                                        NCOMDETAMOUNT :=  (NPERCENT * R_PREMIUM_MO.NPREMIUM) / 100;
                                                    END IF;
                                                END;
                                                END IF;
                                            END;
                                            END IF;

                                        END;

    /*+ TIPO DE MONTO: IMPUESTO   +*/

                                    ELSIF NTYPE_AMOUNT = 4 /* AND SCOMMIS_CO = '1'  */ THEN
                                    BEGIN
                                        IF NVL(R_PREMIUM_MO.NPREMIUM, 0) <> 0 THEN
                                            R_PREMIUM_MO.NPREMIUM := ABS(R_PREMIUM_MO.NPREMIUM);
                                            NCOMDETAMOUNT := NPLUSCOB * R_COMMISS_PR.NAMOUNT / 100;
                                        END IF;
                                    END;
                                    ELSIF NTYPE_AMOUNT = 5 AND
                                          NPLUSOFFICE <> 0 THEN
                                    BEGIN
                                        IF R_PREMIUM_MO.NPREMIUM <> 0        AND
                                           R_PREMIUM_MO.NPREMIUM IS NOT NULL THEN
                                            R_PREMIUM_MO.NPREMIUM := ABS(R_PREMIUM_MO.NPREMIUM);
                                            NCOMDETAMOUNT :=  NPLUSOFFICE * R_COMMISS_PR.NAMOUNT / 100;
                                        END IF;
                                    END;
                                    ELSIF NTYPE_AMOUNT  = 6 AND
                                          NPLUSQUALITY <> 0 THEN
                                    BEGIN
                                        IF R_PREMIUM_MO.NPREMIUM <> 0        AND
                                           R_PREMIUM_MO.NPREMIUM IS NOT NULL THEN
                                            R_PREMIUM_MO.NPREMIUM := ABS(R_PREMIUM_MO.NPREMIUM);
                                            NCOMDETAMOUNT :=  NPLUSQUALITY * R_COMMISS_PR.NAMOUNT / 100;
                                        END IF;
                                    END;
                                    ELSIF NTYPE_AMOUNT = 7 THEN
                                    BEGIN
                                        NSITIVA :=0;
                                        IF NRATEIVA <> 0 AND
                                           NSITIVA   = 1 THEN
                                            NCOMDETAMOUNT :=  NTOTALIVA * NRATEIVA / 100;

                                        ELSIF NVALUEIVA <> 0 THEN
                                            NCOMDETAMOUNT :=  (NTOTALIVA * NRATEIVA / 100) + NVALUEIVA;
                                        END IF;
                                    END;
                                END IF;
                         --   END IF;
--CRETRACE2('INSAGL001 5.2', 984, 'INICIO POR C_TAB_COMPRO ' || TO_CHAR(SYSTIMESTAMP,'dd-mm-yyyy hh24:mi:ss.FF'));
                            IF NVL(NCOMDETAMOUNT, 0) <> 0 THEN
                            BEGIN
                                IF NCOMDETAMOUNT < 0 THEN
                                    NCOMDETAMOUNT :=  NCOMDETAMOUNT * (-1);
                                END IF;

                                NTOTALIVA :=  NCOMDETAMOUNT + NTOTALIVA;


                                    /* ENUNEZ: NOTA IMPORTANTE EL LADO A APLICAR LAS RETENCIONES SIEMPRE SERA EL INVERSO DE COMO    */
                                    /* SE ESCRIBIO EL IMPORTE DE COMISIONES                                                    */

                                    SELECT DECODE (SDEBITSIDE, '1', '2', '1')
                                       INTO SDEBIT_SIDE_RET
                                     FROM DUAL;


/*+ EL PORCENTAJE DE PARTICIPACIÓN (COINSURAN.NSHARE) DE LA COMPAÑÍA LÍDER SE DEBE APLICAR AL MONTO DE COMISIÓN CORRESPONDIENTE Y ASÍ SE OBTIENE EL MONTO*/
/*+ FINAL DE COMISIÓN.*/
--CRETRACE2('INSAGL001 5.3', 984, 'INICIO POR C_TAB_COMPRO ' || TO_CHAR(SYSTIMESTAMP,'dd-mm-yyyy hh24:mi:ss.FF'));
                                IF NVL(R_PREMIUM_MO.SBILLTYPE,'*') = '3' THEN -- NOTA DE CREDITO

                                    BEGIN
                                        SELECT DEFFECDATE
                                          INTO DINITDATE
                                          FROM PREMIUM
                                         WHERE SCERTYPE       = '2'
                                           AND NRECEIPT       = R_PREMIUM_MO.NRECEIPTLINKED
                                           AND NDIGIT         = 0
                                           AND NPAYNUMBE      = 0
                                           AND SSTATUSVA NOT IN ('2', '3');
                                    EXCEPTION
                                        WHEN OTHERS THEN
                                            DINITDATE := R_PREMIUM_MO.DEFFECDATE;
                                    END;

                                ELSE
                                    DINITDATE := R_PREMIUM_MO.DEFFECDATE;
                                END IF;

                                BEGIN
                                    SELECT NSHARE
                                      INTO NSHARE_COIN
                                      FROM COINSURAN
                                     WHERE SCERTYPE    = '2'
                                       AND NBRANCH     = R_PREMIUM_MO.NBRANCH
                                       AND NPRODUCT    = R_PREMIUM_MO.NPRODUCT
                                       AND NPOLICY     = R_PREMIUM_MO.NPOLICY
                                       AND NCOMPANY    = INSAGL001.NCOMPANY
                                       AND DEFFECDATE <= INSAGL001.DINITDATE
                                       AND (DNULLDATE IS NULL
                                        OR DNULLDATE   > INSAGL001.DINITDATE);

                                EXCEPTION
                                    WHEN OTHERS THEN
                                        NSHARE_COIN := 0;
                                END;

                                IF NVL(NSHARE_COIN,0) > 0 THEN
                                    NCOMDETAMOUNT := ((NCOMDETAMOUNT * NSHARE_COIN) / 100);
                                END IF;


--CRETRACE2('INSAGL001 5.4', 984, 'INICIO POR C_TAB_COMPRO ' || TO_CHAR(SYSTIMESTAMP,'dd-mm-yyyy hh24:mi:ss.FF'));

                                /* SE SEPARA LA BUSQUEDA DE ALGUNDOS DATOS DE LA RUTINA DE RETENCIONES DE IIBB PARA MEJORAR EL PERFORMANCE DE LA MISMA  */
                                /* Y EVITAR LA BUSQUEDA DE INFORMACION RELACIONADA CON El INTERMEDIARIO POR CADA DETALLE DEL RECIBO Y PROVINCIA         */
                                /* RELACIONADA A LA RETENCION                                                                                           */

                                        /* SE OBTIENE LA PROVINCIA (NPROVINCE) ASOCIADA A LA DIRECCI¿N PARTICULAR DEL INTERMEDIARIO EN TRATAMIENTO DE LA TABLA DE           */
                                        /*  DIRECCIONES (ADDRESS).                                                                                                          */
                                        BEGIN
                                                SELECT  A.NPROVINCE
                                                  INTO  NPROVINCE_IIBB_INTER
                                                  FROM  ADDRESS A
                                            INNER JOIN  CLIENT C
                                                    ON  A.sclient       =   C.sclient
                                                   AND  A.SRECTYPE      =   '2'
                                                   AND  DEFFECDATE     <=   R_PREMIUM_MO.DCOMPDATE
                                                   AND (DNULLDATE       IS NULL
                                                    OR  DNULLDATE       >   R_PREMIUM_MO.DCOMPDATE)
                                            INNER JOIN  INTERMEDIA I
                                                    ON  I.SCLIENT       =   C.SCLIENT
                                                 WHERE  I.NINTERMED     =   R_COMMISS_PR.NINTERMED;
                                        EXCEPTION
                                            WHEN NO_DATA_FOUND THEN
                                                RAISE_APPLICATION_ERROR(-20000,'EL Productor: ' || R_COMMISS_PR.NINTERMED  ||
                                                                        ' no posee Direcci¿n Particular o bien no es valida para la fecha en tratamiento: '
                                                                        || TO_CHAR(R_PREMIUM_MO.DCOMPDATE));
                                            WHEN OTHERS THEN
                                                RAISE;
                                        END;

                                        /* SE OBTIENEN LAS CONDICIONES DE INGRESOS BRUTOS PARA EL CLIENTE EN TRATAMIENTO, CONDICI¿N DE SUJETO NO ALCANZADO (SNOGROSSINCO),  */
                                        /* CONDICI¿N DE CONVENIO MULTILATERAL (SMULTICONVENTION) DE LA TABLA DE SITUACI¿N IMPOSITIVA DEL CLIENTE (CLIENT_TAX_SITUA). SI EL  */
                                        /* CLIENTE TIENE CONDICI¿N DE SUJETO NO ALCANZADO (SNOGROSSINCO = 1), EL IMPORTE DE RETENCI¿N DE INGRESO BRUTO SER¿ IGUAL A CERO    */
                                        /* (IIBBAMOUNT = 0), EN CASO CONTRARIO, SE CONTIN¿A CON EL PROCESO                                                                  */
                                        BEGIN
                                            SELECT  SIIBBCOND
                                              INTO  SIIBBCOND
                                              FROM  CLIENT_TAX_SITUA
                                             WHERE  SCLIENT             =   INSAGL001.SCLIENT;

                                        EXCEPTION
                                            WHEN NO_DATA_FOUND THEN
                                                RAISE_APPLICATION_ERROR(-20001,'EL Cliente: ' || INSAGL001.SCLIENT  ||
                                                                        ' No posee Situaci¿n impositiva');
                                            WHEN OTHERS THEN
                                                RAISE;
                                        END;


                                         IF (SIIBBCOND = '4') THEN  --SUJETO NO ALCANZADO
                                                IIBBAMOUNT:=0;
                                         ELSE
                                            /*  SE OBTIENEN LAS CONDICIONES DE INGRESOS BRUTOS, INDICADOR DE INSCRIPCI¿N EN LA PROVINCIA (SINCRIPTION), PORCENTAJE DE       */
                                            /* EXENCI¿N (NRATEEXEMIB), FECHA DESDE DE EXENCI¿N (DEXEMIB_FROM), FECHA HASTA DE EXENCI¿N (DEXEMIB_TO) DE LA TABLA DE          */
                                            /* CONDICIONES DE INGRESOS BRUTOS DEL CLIENTE  (CLIENT_TAX_IIBB) PARA LA PROVINCIA DEL INTERMEDIARIO                            */
                                            BEGIN
                                                SELECT  SINSCRIPTION,           NRATEEXEMIB,            DEXEMIB_FROM,
                                                            DEXEMIB_TO, NACTIVITYLEVEL
                                                  INTO  SINSCRIPTION,           NRATEEXEMIB,            DEXEMIB_FROM,
                                                        DEXEMIB_TO, NACTIVITYLEVEL
                                                  FROM  CLIENT_TAX_IIBB
                                                 WHERE  SCLIENT         = INSAGL001.SCLIENT
                                                   AND  NPROVINCE       = INSAGL001.NPROVINCE_IIBB_INTER;

                                            EXCEPTION
                                                WHEN NO_DATA_FOUND THEN
                                                  /*  RAISE_APPLICATION_ERROR(-20002,'EL Cliente: ' || INSAGL001.SCLIENT  ||
                                                                            ' No posee condiciones de Ingresos Brutos para la provincia: ' ||
                                                                            INSAGL001.NPROVINCE_IIBB_INTER);*/
                                                SINSCRIPTION    := 0;
                                                NRATEEXEMIB     := NULL;
                                                DEXEMIB_FROM    := NULL;
                                                DEXEMIB_TO      := NULL;
                                                NACTIVITYLEVEL := 0;

                                                WHEN OTHERS THEN
                                                    RAISE;
                                            END;


                                            IF INSAGL001.NIBRETTYPE <> 1 THEN
                                                /* SE OBTIENE LA AL¿CUOTA DE CONVENIO MULTILATERAL (NMATAX), AL¿CUOTA INSCRIPTO (NREGTAX), LA AL¿CUOTA NO INSCRIPTO (NNOTREGTAX)*/
                                                /* Y EL PORCENTAJE DE DISTRIBUCI¿N DEL INTERMEDIARIO (NINTERSHARE), DE LA TABLA DE RETENCIONES DE INGRESOS BRUTOS (IBRETENTION) */
                                                /* PARA LA PROVINCIA DEL INTERMEDIARIO                                                                                          */
                                                BEGIN
                                                    SELECT  IR.NMATAX,                 IR.NREGTAX,                IR.NNOTREGTAX,
                                                                 IR.NINTERSHARE,         IR.NINCREASEPENALTY, IR.NFIXEDPENALTY
                                                      INTO  NMATAX,                 NREGTAX,                NNOTREGTAX,
                                                               NINTERSHARE,         NINCREASEPENALTY, NFIXEDPENALTY
                                                      FROM  IBRETENTION IR
                                                     WHERE  IR.NPROVINCE       = INSAGL001.NPROVINCE_IIBB_INTER
                                                         AND  IR.NIBRETTYPE      = INSAGL001.NIBRETTYPE
                                                         AND IR.NACTIVITYLEVEL = DECODE(INSAGL001.NIBRETTYPE, 3,  0, INSAGL001.NACTIVITYLEVEL)
                                                         AND  IR.DEFFECDATE     <= R_PREMIUM_MO.DCOMPDATE
                                                         AND (IR.DNULLDATE       IS NULL
                                                           OR  IR.DNULLDATE       > R_PREMIUM_MO.DCOMPDATE);

                                                EXCEPTION
                                                    WHEN NO_DATA_FOUND THEN
                                                        RAISE_APPLICATION_ERROR(-20003,'EL Cliente: ' || INSAGL001.SCLIENT  ||
                                                                                ' No posee Retenciones de IIBB');
                                                    WHEN OTHERS THEN
                                                        RAISE;
                                                END;
                                            END IF;
--CRETRACE2('INSAGL001 5.5', 984, 'INICIO POR C_TAB_COMPRO ' || TO_CHAR(SYSTIMESTAMP,'dd-mm-yyyy hh24:mi:ss.FF'));
                                                /*  SE OBTIENE LA PRIMA FACTURADA DE TODAS LAS COBERTURAS QUE EST¿N ASOCIADAS AL RECIBO EN TRATAMIENTO M¿S EL IMPORTE FACTURADO */
                                                /* DE LOS RECARGOS/DESCUENTOS/IMPUESTOS QUE SE FACTUREN EN EL MISMO CONCEPTO DE FACTURACI¿N DE LAS COBERTURAS, DE LA TABLA DE   */
                                                /* RECIBOS (PREMIUM).                                                                                                           */
                                                BEGIN
                                                    SELECT  SUM(NVL(DP.NPREMIUM,0))
                                                      INTO  SUM_NPREMIUM
                                                      FROM  DETAIL_PRE DP
                                                     WHERE  DP.SCERTYPE     = R_PREMIUM_MO.SCERTYPE
                                                       AND  DP.NBRANCH      = R_PREMIUM_MO.NBRANCH
                                                       AND  DP.NPRODUCT     = R_PREMIUM_MO.NPRODUCT
                                                       AND  DP.NRECEIPT     = R_PREMIUM_MO.NRECEIPT
                                                       AND  DP.NDIGIT       = R_PREMIUM_MO.NDIGIT
                                                       AND  DP.NPAYNUMBE    = R_PREMIUM_MO.NPAYNUMBE
                                                       AND  DP.STYPE_DETAI  = '1';

                                                EXCEPTION
                                                      WHEN  OTHERS THEN
                                                            SUM_NPREMIUM    :=0;
                                                END;

                                                BEGIN
                                                    SELECT  SUM(NVL(DP.NPREMIUM,0))
                                                      INTO  SUM_DISCO_EXPR
                                                      FROM  DETAIL_PRE DP
                                                     WHERE  DP.SCERTYPE     = R_PREMIUM_MO.SCERTYPE
                                                       AND  DP.NBRANCH      = R_PREMIUM_MO.NBRANCH
                                                       AND  DP.NPRODUCT     = R_PREMIUM_MO.NPRODUCT
                                                       AND  DP.NRECEIPT     = R_PREMIUM_MO.NRECEIPT
                                                       AND  DP.NDIGIT       = R_PREMIUM_MO.NDIGIT
                                                       AND  DP.NPAYNUMBE    = R_PREMIUM_MO.NPAYNUMBE
                                                       AND  DP.STYPE_DETAI  <> '1'
                                                       AND  DP.NBILL_ITEM   = (SELECT   DISTINCT(DP2.NBILL_ITEM)
                                                                                 FROM   DETAIL_PRE DP2
                                                                                WHERE   SCERTYPE    =  R_PREMIUM_MO.SCERTYPE
                                                                                  AND   NBRANCH     =   R_PREMIUM_MO.NBRANCH
                                                                                  AND   NPRODUCT    =   R_PREMIUM_MO.NPRODUCT
                                                                                  AND   NRECEIPT    =   R_PREMIUM_MO.NRECEIPT
                                                                                  AND   NDIGIT      =   R_PREMIUM_MO.NDIGIT
                                                                                  AND   NPAYNUMBE   =   R_PREMIUM_MO.NPAYNUMBE
                                                                                  AND   STYPE_DETAI =   '1');

                                                EXCEPTION
                                                      WHEN  OTHERS THEN
                                                            SUM_DISCO_EXPR  :=0;
                                                END;

                                                NREC_AMOUNT := NVL(SUM_NPREMIUM,0) + NVL(SUM_DISCO_EXPR,0);
                                         END IF;

                                /* FIN DE BUSQUEDA DE DATOS PARA LA RUTINA DE RETENCIINES DE IIBB  */

                                BEGIN
                                    NEXCHANGE := NVL(GETEXCHANGE (R_PREMIUM_MO.NCURRENCY, R_PREMIUM_MO.DCOMPDATE),1);
                                END;

/*SELECT COUNT(*) INTO SARA FROM MOVE_ACC WHERE NRECEIPT = 1000070296;
CRETRACE2('INSAGL001 1', 984, ' NTYPE_MOVE ' || NTYPE_MOVE  ||  ' SARA ' || SARA );*/

                                    BEGIN
                                        SELECT NVL(MAX(NVL(NIDCONSEC,0)), 0)
                                          INTO NIDCONSEC
                                          FROM MOVE_ACC
                                         WHERE NTYP_ACCO = INSAGL001.NTYP_ACCO
                                           AND STYPE_ACC = '0'
                                           AND SCLIENT   = INSAGL001.SCLIENT
                                           AND NCURRENCY = R_PREMIUM_MO.NCURRENCY
                                           AND DOPERDATE = INSAGL001.DDATE_END_AUX;
                                    EXCEPTION
                                        WHEN NO_DATA_FOUND THEN
                                            NIDCONSEC := 0;
                                    END;

                                    NIDCONSEC :=  NIDCONSEC + 1;

                                IF NIDCONSEC > 0 OR NIDCONSEC IS NULL THEN
                                BEGIN
                                   /* BEGIN
                                        SELECT NVL(MAX(NVL(NIDCONSEC,0)), 0)
                                          INTO NIDCONSEC
                                          FROM MOVE_ACC
                                         WHERE NTYP_ACCO = INSAGL001.NTYP_ACCO
                                           AND STYPE_ACC = '0'
                                           AND SCLIENT   = INSAGL001.SCLIENT
                                           AND NCURRENCY = R_PREMIUM_MO.NCURRENCY
                                           AND DOPERDATE = INSAGL001.DDATE_END_AUX;
                                    EXCEPTION
                                        WHEN NO_DATA_FOUND THEN
                                            NIDCONSEC := 0;
                                    END;

                                    NIDCONSEC :=  NIDCONSEC + 1;*/
                                     IF SDEBITSIDE = '1' THEN
                                        NAMODEB  :=  NCOMDETAMOUNT;
                                        NAMOCRED :=  0;
                                    ELSE
                                        NAMODEB  :=  0;
                                        NAMOCRED :=  NCOMDETAMOUNT;
                                    END IF;


                                    NCOMDETAMOUNT_MA := NCOMDETAMOUNT;

                                    IF NTYPE_AMOUNT = 1 AND
                                       NTYPE_TRAN   = 1 THEN
                                    BEGIN
                                        SELECT SDESCRIPT
                                          INTO SDESCRIPT
                                          FROM TABLE6
                                         WHERE NTYPE_TRAN = 1;
                                    EXCEPTION
                                        WHEN NO_DATA_FOUND THEN
                                            SDESCRIPT := NULL;
                                    END;
                                    ELSE
                                    BEGIN
                                        SELECT SDESCRIPT
                                          INTO SDESCRIPT
                                          FROM TABLE401
                                         WHERE NTYPE_MOVE = INSAGL001.NTYPE_MOVE
                                           AND SSTATREGT  = '1';
                                    EXCEPTION
                                        WHEN NO_DATA_FOUND THEN
                                            NULL;
                                    END;
                                    END IF;
                                    

                                    NIDCONSEC :=  NVL(NIDCONSEC, 0) + 1;

                                    IF NTYPE_MOVE = 401 THEN
                                    BEGIN
                                     INSMOVE_ACC (NTYP_ACCO   => NTYP_ACCO,
                                                            NCURRENCY   => R_PREMIUM_MO.NCURRENCY,                                                    
                                                            SCLIENT     => SCLIENT,
                                                            NTYPE_MOVE  => NTYPE_MOVE, 
                                                            STYPE_ACC   => '0',
                                                            IN_DOPERDATE   => R_PREMIUM_MO.DCOMPDATE,
                                                            NREM_NUMBER   => NIDCONSEC,
                                                            NINTERMED   => R_COMMISS_PR.NINTERMED,
                                                            NAMOUNT     => NCOMDETAMOUNT_MA,  
                                                            SDESCRIPT   => SDESCRIPT_MINI,
                                                            NBRANCH     => R_PREMIUM_MO.NBRANCH,
                                                            NPOLICY     => R_PREMIUM_MO.NPOLICY,
                                                            NPRODUCT    => R_PREMIUM_MO.NPRODUCT,
                                                            NTYPE_PAY   => 3,
                                                            NCREDIT     => NAMOCRED, 
                                                            NTYPE_TRAN  => NTYPE_TRAN,
                                                            NDEBIT      => NAMODEB, 
                                                            NTRANSACTIO => R_PREMIUM_MO.NTRANSAC,
                                                            NUSERCODE   => NUSERCODE,
                                                            NPROVINCE   => R_COMMISS_PR.NPROVINCE,
                                                            NRECEIPT    => R_PREMIUM_MO.NRECEIPT,
                                                            NAMOAFECT   => (CASE WHEN NCOMDETAMOUNT_MA < 0 THEN  NCOM_AFEC_AUX * -1 ELSE NCOM_AFEC_AUX END), --NCOM_AFEC 
                                                            NAMOEXEN    => (CASE WHEN NCOMDETAMOUNT_MA < 0 THEN  NCOM_EXEN_AUX * -1 ELSE NCOM_EXEN_AUX END), --NCOM_EXEN
                                                            NCOMMIT     => 2,
                                                            NPAYNUMBE   => R_PREMIUM_MO.NPAYNUMBE,
                                                            NCERTIF     => R_PREMIUM_MO.NCERTIF,
                                                            NEXCHANGE   => NEXCHANGE,
                                                            NINTERTYP   => NINTERTYP,
                                                            NCURR_ACC   => NACCOKEY,
                                                            NORIGCURR   => R_PREMIUM_MO.NCURRENCY,
                                                            SCERTYPE     => R_PREMIUM_MO.SCERTYPE,
                                                            NDIGIT          => R_PREMIUM_MO.NDIGIT);
                                                    
                                                    NIDCONSEC := NIDCONSEC + 1; 

                                    END;
                                    ELSIF NTYPE_MOVE = 404 THEN
                                    BEGIN

                                     INSMOVE_ACC(NTYP_ACCO   => NTYP_ACCO,
                                                            NCURRENCY   => R_PREMIUM_MO.NCURRENCY,                                                    
                                                            SCLIENT     => SCLIENT,
                                                            NTYPE_MOVE  => NTYPE_MOVE, 
                                                            STYPE_ACC   => '0',
                                                            IN_DOPERDATE   => R_PREMIUM_MO.DCOMPDATE,
                                                            NREM_NUMBER   => NIDCONSEC,
                                                            NINTERMED   => R_COMMISS_PR.NINTERMED,
                                                            NAMOUNT     => (NCOMDETAMOUNT_MA * -1),
                                                            SDESCRIPT   => SDESCRIPT_MINI,
                                                            NBRANCH     => R_PREMIUM_MO.NBRANCH,
                                                            NPOLICY     => R_PREMIUM_MO.NPOLICY,
                                                            NPRODUCT    => R_PREMIUM_MO.NPRODUCT,
                                                            NTYPE_PAY   => 3,
                                                            NCREDIT     => NAMOCRED, 
                                                            NTYPE_TRAN  => NTYPE_TRAN,
                                                            NDEBIT      => NAMODEB, 
                                                            NTRANSACTIO =>  R_PREMIUM_MO.NTRANSAC,
                                                            NUSERCODE   => NUSERCODE,
                                                            NPROVINCE   => R_COMMISS_PR.NPROVINCE,
                                                            NRECEIPT    => R_PREMIUM_MO.NRECEIPT,
                                                            NAMOAFECT   => (CASE WHEN NCOMDETAMOUNT_MA < 0 THEN  NCOM_AFEC_AUX * -1 ELSE NCOM_AFEC_AUX END), --NCOM_AFEC 
                                                            NAMOEXEN    => (CASE WHEN NCOMDETAMOUNT_MA < 0 THEN  NCOM_EXEN_AUX * -1 ELSE NCOM_EXEN_AUX END), --NCOM_EXEN
                                                            NCOMMIT     => 2,
                                                            NPAYNUMBE   => R_PREMIUM_MO.NPAYNUMBE,
                                                            NCERTIF     => R_PREMIUM_MO.NCERTIF,
                                                            NEXCHANGE   => NEXCHANGE,
                                                            NINTERTYP   => NINTERTYP,
                                                            NCURR_ACC   => NACCOKEY,
                                                            NORIGCURR   => R_PREMIUM_MO.NCURRENCY,
                                                            SCERTYPE     => R_PREMIUM_MO.SCERTYPE,
                                                            NDIGIT          => R_PREMIUM_MO.NDIGIT);
                                                    
                                                    NIDCONSEC := NIDCONSEC + 1;
                                    END;

                                    ELSE
                                    BEGIN

                                     INSMOVE_ACC(NTYP_ACCO   => NTYP_ACCO,
                                                            NCURRENCY   => R_PREMIUM_MO.NCURRENCY,                                                    
                                                            SCLIENT     => SCLIENT,
                                                            NTYPE_MOVE  => NTYPE_MOVE, 
                                                            STYPE_ACC   => '0',
                                                            IN_DOPERDATE   => R_PREMIUM_MO.DCOMPDATE,
                                                            NREM_NUMBER   => NIDCONSEC,
                                                            NINTERMED   => R_COMMISS_PR.NINTERMED,
                                                            NAMOUNT     => NCOMDETAMOUNT,
                                                            SDESCRIPT   => SDESCRIPT_MINI,
                                                            NBRANCH     => R_PREMIUM_MO.NBRANCH,
                                                            NPOLICY     => R_PREMIUM_MO.NPOLICY,
                                                            NPRODUCT    => R_PREMIUM_MO.NPRODUCT,
                                                            NTYPE_PAY   => 3,
                                                            NCREDIT     => NAMOCRED, 
                                                            NTYPE_TRAN  => NTYPE_TRAN,
                                                            NDEBIT      => NAMODEB, 
                                                            NTRANSACTIO =>  R_PREMIUM_MO.NTRANSAC,
                                                            NUSERCODE   => NUSERCODE,
                                                            NPROVINCE   => R_COMMISS_PR.NPROVINCE,
                                                            NRECEIPT    => R_PREMIUM_MO.NRECEIPT,
                                                            NAMOAFECT   => R_COMMISS_PR.NCOM_AFEC, --NCOM_AFEC,
                                                            NAMOEXEN    => R_COMMISS_PR.NCOM_EXEN, --NCOM_EXEN,
                                                            NCOMMIT     => 2,
                                                            NPAYNUMBE   => R_PREMIUM_MO.NPAYNUMBE,
                                                            NCERTIF     => R_PREMIUM_MO.NCERTIF,
                                                            NEXCHANGE   => NEXCHANGE,
                                                            NINTERTYP   => NINTERTYP,
                                                            NCURR_ACC   => NACCOKEY,
                                                            NORIGCURR   => R_PREMIUM_MO.NCURRENCY,
                                                            SCERTYPE     => R_PREMIUM_MO.SCERTYPE,
                                                            NDIGIT          => R_PREMIUM_MO.NDIGIT);
                                                    
                                                    NIDCONSEC := NIDCONSEC + 1;
                                    END;
                                    END IF;


    /*+ PARA CALCULAR LA RETENCI¿N DE IMPUESTOS DE SERVICIOS SOCIALES DE SEGUROS, SE HACE EL LLAMADO A LA RUTINA DE RETENCI¿N    */
    /*  DE IMPUESTOS DE SERVICIOS SOCIALES DE SEGUROS.                                                                           +*/
--CRETRACE2('INSAGL001 6', 984, 'INICIO INSCALISSS ' || TO_CHAR(SYSTIMESTAMP,'dd-mm-yyyy hh24:mi:ss.FF'));
                                    INSCALISSS(NTYP_ACCO    => NTYP_ACCO,
                                                       STYPE_ACC    => '0',
                                                       SCLIENT      => SCLIENT,
                                                       NCURRENCY    => R_PREMIUM_MO.NCURRENCY,
                                                       DOPERDATE    => R_PREMIUM_MO.DCOMPDATE,--DDATE_END_AUX,,
                                                       NIDCONSEC    => NIDCONSEC,
                                                       NACCO_KEY    => NACCOKEY, --NPAYPROCID
                                                       NINTERTYP    => NINTERTYP,
                                                       NINTERMED    => R_COMMISS_PR.NINTERMED,
                                                       NPROVINCE    => R_COMMISS_PR.NPROVINCE,
                                                       NCREDIT      => NAMOCRED,
                                                       NDEBIT       => NAMODEB,
                                                       NUSERCODE    => NUSERCODE,
                                                       SCERTYPE     => R_PREMIUM_MO.SCERTYPE,
                                                       NBRANCH      => R_PREMIUM_MO.NBRANCH,
                                                       NPRODUCT     => R_PREMIUM_MO.NPRODUCT,
                                                       NRECEIPT     => R_PREMIUM_MO.NRECEIPT,
                                                       NDIGIT       => R_PREMIUM_MO.NDIGIT,
                                                       NPAYNUMBE    => R_PREMIUM_MO.NPAYNUMBE,
                                                       SKEY         => INSAGL001.SKEY,
                                                       NEXCHANGE    => INSAGL001.NEXCHANGE,
                                                       NORIGCURR    => R_PREMIUM_MO.NCURRENCY,
                                                       NCOM_AFEC    => R_COMMISS_PR.NCOM_AFEC,
                                                       NCOM_EXEN    => R_COMMISS_PR.NCOM_EXEN,
                                                       NCOM_NOCLAS  => 0,
                                                       NAMOUNT_MA   => NAMOUNT_RET,
                                                       NCREDIT_MA   => NCREDIT_RET,
                                                       NDEBIT_MA    => NDEBIT_RET,
                                                       NID_MOVE_ACC => NID_MOVE_ACC,
                                                       SDEBIT_SIDE_RET => SDEBIT_SIDE_RET,--(CASE WHEN NTYPE_MOVE = 404 THEN '2' ELSE '1' END),
                                                       NCOMIDCONSEC => INSAGL001.NAUXNULL,
                                                       NADJUSTMENTTYPE => R_COMMISS_PR.NADJUSTMENTTYPE_AUX);
--CRETRACE2('INSAGL001 7', 984, 'FIN INSCALISSS ' || TO_CHAR(SYSTIMESTAMP,'dd-mm-yyyy hh24:mi:ss.FF'));

                                    IF (SIIBBCOND <> '4') THEN
    /*+  PARA CALCULAR LA RETENCI¿N DE INGRESOS BRUTOS, SE HACE EL LLAMADO A LA RUTINA DE C¿LCULO DE RETENCI¿N DE INGRESOS BRUTOS.  +*/
--CRETRACE2('INSAGL001 8', 984, 'INICIO INSCALRETIIBB ' || TO_CHAR(SYSTIMESTAMP,'dd-mm-yyyy hh24:mi:ss.FF'));
                                        IF NIBRETTYPE = 2  THEN
--CRETRACE2('INSAGL001 8.1', 984, 'INICIO INSCALRETIIBB  NIBRETTYPE = 2 ' || TO_CHAR(SYSTIMESTAMP,'dd-mm-yyyy hh24:mi:ss.FF'));
                                            INSCALRETIIBB(NTYP_ACCO                 =>  NTYP_ACCO,
                                                                  STYPE_ACC                 => '0',
                                                                  DOPERDATE                 => R_PREMIUM_MO.DCOMPDATE,--DDATE_END_AUX,,
                                                                  NACCO_KEY                 => NACCOKEY,
                                                                  NCURRENCY                 => R_PREMIUM_MO.NCURRENCY,
                                                                  NIDCONSEC                 => NIDCONSEC,
                                                                  NINTERTYP                 => NINTERTYP,
                                                                  NINTERMED                 => R_COMMISS_PR.NINTERMED,
                                                                  NCREDIT                   => NAMOCRED,
                                                                  NDEBIT                    => NAMODEB,
                                                                  NRECEIPT                  => R_PREMIUM_MO.NRECEIPT,
                                                                  NDIGIT                    => R_PREMIUM_MO.NDIGIT,
                                                                  NPAYNUMBE                 => R_PREMIUM_MO.NPAYNUMBE,
                                                                  NAMOUNT                   => NCOMDETAMOUNT,
                                                                  SCERTYPE                  => R_PREMIUM_MO.SCERTYPE,
                                                                  NBRANCH                   => R_PREMIUM_MO.NBRANCH,
                                                                  NPRODUCT                  => R_PREMIUM_MO.NPRODUCT,

                                                                  NPROVINCE                 => INSAGL001.NPROVINCE_IIBB_INTER,
                                                                  SCLIENT                   => INSAGL001.SCLIENT,
                                                                  SIIBBCOND                 => INSAGL001.SIIBBCOND,
                                                                  SINSCRIPTION              => INSAGL001.SINSCRIPTION,
                                                                  NRATEEXEMIB               => INSAGL001.NRATEEXEMIB,
                                                                  DEXEMIB_FROM              => INSAGL001.DEXEMIB_FROM,
                                                                  DEXEMIB_TO                => INSAGL001.DEXEMIB_TO,
                                                                  NMATAX                    => INSAGL001.NMATAX,
                                                                  NREGTAX                   => INSAGL001.NREGTAX,
                                                                  NNOTREGTAX                => INSAGL001.NNOTREGTAX,
                                                                  NINTERSHARE               => INSAGL001.NINTERSHARE,
                                                                  SUM_NPREMIUM              => INSAGL001.SUM_NPREMIUM,
                                                                  SUM_DISCO_EXPR            => INSAGL001.SUM_DISCO_EXPR,
                                                                  NREC_AMOUNT               => INSAGL001.NREC_AMOUNT,
                                                                  IIBBAMOUNT                => INSAGL001.IIBBAMOUNT,
                                                                  SKEY                      => INSAGL001.SKEY,
                                                                  NEXCHANGE                 => INSAGL001.NEXCHANGE,
                                                                  NORIGCURR                 => R_PREMIUM_MO.NCURRENCY,
                                                                  NCOM_AFEC                 => R_COMMISS_PR.NCOM_AFEC,
                                                                  NCOM_EXEN                 => R_COMMISS_PR.NCOM_EXEN,
                                                                  NUSERCODE                 => NUSERCODE,
                                                                  NID_MOVE_ACC              => NID_MOVE_ACC,
                                                                  SDEBIT_SIDE_RET           => SDEBIT_SIDE_RET,--(CASE WHEN NTYPE_MOVE = 404 THEN '2' ELSE '1' END),
                                                                  NCOMIDCONSEC              => INSAGL001.NAUXNULL,
                                                                  NADJUSTMENTTYPE => R_COMMISS_PR.NADJUSTMENTTYPE_AUX,
                                                                  NINCREASEPENALTY   =>        INSAGL001.NINCREASEPENALTY,
                                                                  NFIXEDPENALTY        =>       INSAGL001.NFIXEDPENALTY);
                                                          
--CRETRACE2('INSAGL001 8.1', 984, 'fin INSCALRETIIBB  NIBRETTYPE = 2 ' || TO_CHAR(SYSTIMESTAMP,'dd-mm-yyyy hh24:mi:ss.FF'));
                                        ELSIF NIBRETTYPE = 3 THEN
--CRETRACE2('INSAGL001 8.1', 984, 'INICIO INSCALRETIIBB  NIBRETTYPE = 3 ' || TO_CHAR(SYSTIMESTAMP,'dd-mm-yyyy hh24:mi:ss.FF'));
                                            INSCALRETIIBB_RG(NTYP_ACCO                 =>  NTYP_ACCO,
                                                                         STYPE_ACC                 => '0',
                                                                         DOPERDATE                 => R_PREMIUM_MO.DCOMPDATE,--DDATE_END_AUX,,
                                                                         NACCO_KEY                 => NACCOKEY,
                                                                         SCLIENT                   => INSAGL001.SCLIENT,
                                                                         NAMOUNT                   => NVL(NCOMDETAMOUNT_MA,0),
                                                                         IIBBAMOUNT                => INSAGL001.IIBBAMOUNT,
                                                                         NVATAFFECTYPE             => (CASE WHEN NVL(INSAGL001.NCOM_AFEC_AUX,0) <> 0 THEN 1 ELSE 2 END),
                                                                         NCURRENCY                 => R_PREMIUM_MO.NCURRENCY,
                                                                         NID_MOVE_ACC              => NID_MOVE_ACC,
                                                                         SDEBIT_SIDE_RET           => SDEBIT_SIDE_RET,-- (CASE WHEN NTYPE_MOVE = 404 THEN '2' ELSE '1' END),
                                                                         SKEY                      => INSAGL001.SKEY,
                                                                         NUSERCODE                 => NUSERCODE,
                                                                         NEXCHANGE                 => INSAGL001.NEXCHANGE,
                                                                         NINTERTYP                 => NINTERTYP,
                                                                         NINTERMED                 => R_COMMISS_PR.NINTERMED,
                                                                         NBRANCH                   => R_PREMIUM_MO.NBRANCH,
                                                                         NPRODUCT                  => R_PREMIUM_MO.NPRODUCT,
                                                                         SCERTYPE                  => R_PREMIUM_MO.SCERTYPE,
                                                                         NRECEIPT                  => R_PREMIUM_MO.NRECEIPT,
                                                                         NADJUSTMENTTYPE => R_COMMISS_PR.NADJUSTMENTTYPE_AUX);
--CRETRACE2('INSAGL001 8.1', 984, 'fin INSCALRETIIBB  NIBRETTYPE = 3 ' || TO_CHAR(SYSTIMESTAMP,'dd-mm-yyyy hh24:mi:ss.FF'));
                                        END IF;
--CRETRACE2('INSAGL001 9', 984, 'FIN INSCALRETIIBB ' || TO_CHAR(SYSTIMESTAMP,'dd-mm-yyyy hh24:mi:ss.FF'));
                                    END IF;

                                    CRETMP_AGL001(SKEY          => SKEY,
                                                              NTYP_ACCO     => NTYP_ACCO,
                                                              STYPE_ACC     => 0,
                                                              SCLIENT       => SCLIENT,
                                                              NCURRENCY     => R_PREMIUM_MO.NCURRENCY,
                                                              DOPERDATE     => R_PREMIUM_MO.DCOMPDATE,--DDATE_END_AUX,
                                                              NINTERMED     => R_COMMISS_PR.NINTERMED,
                                                              NTYPE_MOVE    => NTYPE_MOVE,
                                                              NAMOUNT       => NCOMDETAMOUNT,
                                                              NCREDIT       => NAMOCRED,
                                                              NDEBIT        => NAMODEB,
                                                              NRECEIPT      => R_PREMIUM_MO.NRECEIPT,
                                                              NUSERCODE     => NUSERCODE,
                                                              DCOMPDATE     => R_PREMIUM_MO.DCOMPDATE);

                                                                 
--                                     INSMOVE_ACC (NTYP_ACCO          => NTYP_ACCO,
--                                                                 STYPE_ACC          => '0',--ok
--                                                                 NTYPE_MOVE => NTYPE_MOVE,
--                                                                 SCLIENT            => SCLIENT,
--                                                                 NCURRENCY          => R_PREMIUM_MO.NCURRENCY,
--                                                                 NUSERCODE          => NUSERCODE,--ok
--                                                                 NCOMMIT            => 2,
--                                                                 NREM_NUMBER => NREM_NUMBER);                                                                 


                                END;

    /*+ SI ES UN NIDCONSEC EXISTENTE +*/

                                ELSE
                                BEGIN
                                    IF SDEBITSIDE = '1' THEN
                                        NAMODEB := NCOMDETAMOUNT;
                                        NAMOCRED :=  0;
                                    ELSE
                                        NAMODEB := 0;
                                        NAMOCRED := NCOMDETAMOUNT;
                                    END IF;

    /*+ PARA CALCULAR LA RETENCI¿N DE IMPUESTOS DE SERVICIOS SOCIALES DE SEGUROS, SE HACE EL LLAMADO A LA RUTINA DE RETENCI¿N DE IMPUESTOS DE SERVICIOS SOCIALES DE SEGUROS. +*/
--CRETRACE2('INSAGL001 10', 984, 'INICIO INSCALISSS ' || TO_CHAR(SYSTIMESTAMP,'dd-mm-yyyy hh24:mi:ss.FF'));
                                    INSCALISSS(NTYP_ACCO    => NTYP_ACCO,
                                               STYPE_ACC    => '0',
                                               SCLIENT      => SCLIENT,
                                               NCURRENCY    => R_PREMIUM_MO.NCURRENCY,
                                               DOPERDATE    => R_PREMIUM_MO.DCOMPDATE,--DDATE_END_AUX,
                                               NIDCONSEC    => NIDCONSEC,
                                               NACCO_KEY    => NACCOKEY, --NPAYPROCID
                                               NINTERTYP    => NINTERTYP,
                                               NINTERMED    => R_COMMISS_PR.NINTERMED,
                                               NPROVINCE    => R_COMMISS_PR.NPROVINCE,
                                               NCREDIT      => NAMOCRED,
                                               NDEBIT       => NAMODEB,
                                               NUSERCODE    => NUSERCODE,
                                               SCERTYPE     => R_PREMIUM_MO.SCERTYPE,
                                               NBRANCH      => R_PREMIUM_MO.NBRANCH,
                                               NPRODUCT     => R_PREMIUM_MO.NPRODUCT,
                                               NRECEIPT     => R_PREMIUM_MO.NRECEIPT,
                                               NDIGIT       => R_PREMIUM_MO.NDIGIT,
                                               NPAYNUMBE    => R_PREMIUM_MO.NPAYNUMBE,
                                               SKEY         => INSAGL001.SKEY,
                                               NEXCHANGE    => INSAGL001.NEXCHANGE,
                                               NORIGCURR    => R_PREMIUM_MO.NCURRENCY,
                                               NCOM_AFEC    => R_COMMISS_PR.NCOM_AFEC,
                                               NCOM_EXEN    => R_COMMISS_PR.NCOM_EXEN,
                                               NCOM_NOCLAS  => 0,
                                               NAMOUNT_MA   => NAMOUNT_RET,
                                               NCREDIT_MA   => NCREDIT_RET,
                                               NDEBIT_MA    => NDEBIT_RET,
                                               NID_MOVE_ACC => NID_MOVE_ACC,
                                               SDEBIT_SIDE_RET => SDEBIT_SIDE_RET,--(CASE WHEN NTYPE_MOVE = 404 THEN '2' ELSE '1' END),
                                               NCOMIDCONSEC => INSAGL001.NAUXNULL,
                                               NADJUSTMENTTYPE => R_COMMISS_PR.NADJUSTMENTTYPE_AUX);
--CRETRACE2('INSAGL001 11', 984, 'FIN INSCALISSS ' || TO_CHAR(SYSTIMESTAMP,'dd-mm-yyyy hh24:mi:ss.FF'));
    /*+  PARA CALCULAR LA RETENCI¿N DE INGRESOS BRUTOS, SE HACE EL LLAMADO A LA RUTINA DE C¿LCULO DE RETENCI¿N DE INGRESOS BRUTOS.  +*/

                                    IF (SIIBBCOND <> '4') THEN
--CRETRACE2('INSAGL001 12', 984, 'INICIO INSCALRETIIBB ' || TO_CHAR(SYSTIMESTAMP,'dd-mm-yyyy hh24:mi:ss.FF'));
                                        IF NIBRETTYPE = 2 THEN
                                            INSCALRETIIBB(NTYP_ACCO                 =>  INSAGL001.NTYP_ACCO,
                                                          STYPE_ACC                 => '0',
                                                          DOPERDATE                 => R_PREMIUM_MO.DCOMPDATE,--DDATE_END_AUX,
                                                          NACCO_KEY                 => NACCOKEY,
                                                          NCURRENCY                 => R_PREMIUM_MO.NCURRENCY,
                                                          NIDCONSEC                 => INSAGL001.NIDCONSEC,
                                                          NINTERTYP                 => INSAGL001.NINTERTYP,
                                                          NINTERMED                 => R_COMMISS_PR.NINTERMED,
                                                          NCREDIT                   => NAMOCRED,
                                                          NDEBIT                    => NAMODEB,
                                                          NRECEIPT                  => R_PREMIUM_MO.NRECEIPT,
                                                          NDIGIT                    => R_PREMIUM_MO.NDIGIT,
                                                          NPAYNUMBE                 => R_PREMIUM_MO.NPAYNUMBE,
                                                          NAMOUNT                   => NCOMDETAMOUNT,
                                                          SCERTYPE                  => R_PREMIUM_MO.SCERTYPE,
                                                          NBRANCH                   => R_PREMIUM_MO.NBRANCH,
                                                          NPRODUCT                  => R_PREMIUM_MO.NPRODUCT,
                                                          NPROVINCE                 => INSAGL001.NPROVINCE_IIBB_INTER,
                                                          SCLIENT                   => INSAGL001.SCLIENT,
                                                          SIIBBCOND                 => INSAGL001.SIIBBCOND,
                                                          SINSCRIPTION              => INSAGL001.SINSCRIPTION,
                                                          NRATEEXEMIB               => INSAGL001.NRATEEXEMIB,
                                                          DEXEMIB_FROM              => INSAGL001.DEXEMIB_FROM,
                                                          DEXEMIB_TO                => INSAGL001.DEXEMIB_TO,
                                                          NMATAX                    => INSAGL001.NMATAX,
                                                          NREGTAX                   => INSAGL001.NREGTAX,
                                                          NNOTREGTAX                => INSAGL001.NNOTREGTAX,
                                                          NINTERSHARE               => INSAGL001.NINTERSHARE,
                                                          SUM_NPREMIUM              => INSAGL001.SUM_NPREMIUM,
                                                          SUM_DISCO_EXPR            => INSAGL001.SUM_DISCO_EXPR,
                                                          NREC_AMOUNT               => INSAGL001.NREC_AMOUNT,
                                                          IIBBAMOUNT                => INSAGL001.IIBBAMOUNT,
                                                          SKEY                      => INSAGL001.SKEY,
                                                          NEXCHANGE                 => INSAGL001.NEXCHANGE,
                                                          NORIGCURR                 => R_PREMIUM_MO.NCURRENCY,
                                                          NCOM_AFEC                 => R_COMMISS_PR.NCOM_AFEC,
                                                          NCOM_EXEN                 => R_COMMISS_PR.NCOM_EXEN,
                                                          NUSERCODE                 => NUSERCODE,
                                                          NID_MOVE_ACC              => NID_MOVE_ACC,
                                                          SDEBIT_SIDE_RET           => SDEBIT_SIDE_RET,--(CASE WHEN NTYPE_MOVE = 404 THEN '2' ELSE '1' END),
                                                          NCOMIDCONSEC              => INSAGL001.NAUXNULL,
                                                          NADJUSTMENTTYPE => R_COMMISS_PR.NADJUSTMENTTYPE_AUX,
                                                          NINCREASEPENALTY   =>        INSAGL001.NINCREASEPENALTY,
                                                          NFIXEDPENALTY        =>       INSAGL001.NFIXEDPENALTY);
                                        ELSIF NIBRETTYPE = 3 THEN
                                            INSCALRETIIBB_RG(NTYP_ACCO                 =>  NTYP_ACCO,
                                                             STYPE_ACC                 => '0',
                                                             DOPERDATE                 => R_PREMIUM_MO.DCOMPDATE,--DDATE_END_AUX,
                                                             NACCO_KEY                 => NACCOKEY,
                                                             SCLIENT                   => INSAGL001.SCLIENT,
                                                             NAMOUNT                   => NCOMDETAMOUNT,
                                                             IIBBAMOUNT                => INSAGL001.IIBBAMOUNT,
                                                             NVATAFFECTYPE             => (CASE WHEN INSAGL001.NCOM_AFEC_AUX <> 0 THEN 1 ELSE 2 END),
                                                             NCURRENCY                 => R_PREMIUM_MO.NCURRENCY,
                                                             NID_MOVE_ACC              => NID_MOVE_ACC,
                                                             SDEBIT_SIDE_RET           => SDEBIT_SIDE_RET,--(CASE WHEN NTYPE_MOVE = 404 THEN '2' ELSE '1' END),
                                                             SKEY                      => INSAGL001.SKEY,
                                                             NUSERCODE                 => NUSERCODE,
                                                             NEXCHANGE                 => INSAGL001.NEXCHANGE,
                                                             NINTERTYP                 => NINTERTYP,
                                                             NINTERMED                 => R_COMMISS_PR.NINTERMED,
                                                             NBRANCH                   => R_PREMIUM_MO.NBRANCH,
                                                             NPRODUCT                  => R_PREMIUM_MO.NPRODUCT,
                                                             SCERTYPE                  => R_PREMIUM_MO.SCERTYPE,
                                                             NRECEIPT                  => R_PREMIUM_MO.NRECEIPT,
                                                             NADJUSTMENTTYPE => R_COMMISS_PR.NADJUSTMENTTYPE_AUX);
                                        END IF;
--CRETRACE2('INSAGL001 13', 984, 'FIN INSCALRETIIBB ' || TO_CHAR(SYSTIMESTAMP,'dd-mm-yyyy hh24:mi:ss.FF'));
                                    END IF;

                                    CRETMP_AGL001(SKEY          => SKEY,
                                                  NTYP_ACCO     => NTYP_ACCO,
                                                  STYPE_ACC     => 0,
                                                  SCLIENT       => SCLIENT,
                                                  NCURRENCY     => R_PREMIUM_MO.NCURRENCY,
                                                  DOPERDATE     => R_PREMIUM_MO.DCOMPDATE,--DDATE_END_AUX,
                                                  NINTERMED     => R_COMMISS_PR.NINTERMED,
                                                  NTYPE_MOVE    => NTYPE_MOVE,
                                                  NAMOUNT       => NCOMDETAMOUNT,
                                                  NCREDIT       => NAMOCRED,
                                                  NDEBIT        => NAMODEB,
                                                  NRECEIPT      => R_PREMIUM_MO.NRECEIPT,
                                                  NUSERCODE     => NUSERCODE,
                                                  DCOMPDATE     => R_PREMIUM_MO.DCOMPDATE);

                                    UPDATE MOVE_ACC
                                       SET NCREDIT    = (CASE WHEN R_COMMISS_PR.NADJUSTMENTTYPE_AUX = 3 THEN (NCREDIT + (NAMOCRED * -1)) ELSE (NCREDIT + NAMOCRED) END),
                                           NDEBIT     = (CASE WHEN R_COMMISS_PR.NADJUSTMENTTYPE_AUX = 3 THEN (NDEBIT + (NAMODEB * -1)) ELSE (NDEBIT  + NAMODEB) END),
                                           NAMOUNT    = NAMOUNT + NCOMDETAMOUNT,
                                           SSTATREGT  = '1',
                                           NTYPE_PAY  =  3,
                                           NUSERCODE  = INSAGL001.NUSERCODE,
                                           DCOMPDATE  = SYSDATE,
                                           NAMOAFECT  = (CASE WHEN R_COMMISS_PR.NADJUSTMENTTYPE_AUX = 3 THEN R_COMMISS_PR.NCOM_AFEC * -1 ELSE R_COMMISS_PR.NCOM_AFEC END),
                                           NAMOEXEN   = (CASE WHEN R_COMMISS_PR.NADJUSTMENTTYPE_AUX = 3 THEN R_COMMISS_PR.NCOM_EXEN * -1 ELSE R_COMMISS_PR.NCOM_EXEN END)
                                     WHERE NIDCONSEC  = INSAGL001.NIDCONSEC
                                       AND NTYP_ACCO  = INSAGL001.NTYP_ACCO
                                       AND STYPE_ACC  = '0'
                                       AND NINTERMED  = R_COMMISS_PR.NINTERMED
                                       AND SCLIENT    = INSAGL001.SCLIENT
                                       AND NRECEIPT   = R_PREMIUM_MO.NRECEIPT
                                       AND NCURRENCY  = R_PREMIUM_MO.NCURRENCY
                                       AND DOPERDATE  = INSAGL001.DDATE_END_AUX
                                       AND NTYPE_MOVE = INSAGL001.NTYPE_MOVE;


                                END;
                                END IF;

                                NTOTALCOMMISSION :=  NCOMDETAMOUNT + NTOTALCOMMISSION;

                                BEGIN
                                     SELECT DISTINCT NBILL_ITEM --CONCEPTO DE FACTURACION
                                       INTO NBILL_ITEM_AUX
                                       FROM DETAIL_PRE
                                      WHERE SCERTYPE    = R_PREMIUM_MO.SCERTYPE
                                        AND NBRANCH     = R_PREMIUM_MO.NBRANCH
                                        AND NPRODUCT    = R_PREMIUM_MO.NPRODUCT
                                        AND NRECEIPT    = R_PREMIUM_MO.NRECEIPT
                                        AND NDIGIT      = R_PREMIUM_MO.NDIGIT
                                        AND NPAYNUMBE   = R_PREMIUM_MO.NPAYNUMBE
                                        AND STYPE_DETAI = '1';

                                EXCEPTION
                                    WHEN NO_DATA_FOUND THEN
                                       NBILL_ITEM_AUX := 0;
                                    WHEN TOO_MANY_ROWS THEN
                                       NBILL_ITEM_AUX := 1;
                                    WHEN OTHERS THEN
                                       NBILL_ITEM_AUX := 0;
                                END;

                                BEGIN
                                    NPREM_RECEIPT := 0;

                                    SELECT NVL(SUM(NVL(NPREMIUM,0)),0)
                                      INTO NPREM_RECEIPT
                                      FROM DETAIL_PRE
                                     WHERE SCERTYPE   = R_PREMIUM_MO.SCERTYPE
                                       AND NBRANCH    = R_PREMIUM_MO.NBRANCH
                                       AND NPRODUCT   = R_PREMIUM_MO.NPRODUCT
                                       AND NRECEIPT   = R_PREMIUM_MO.NRECEIPT
                                       AND NDIGIT     = R_PREMIUM_MO.NDIGIT
                                       AND NPAYNUMBE  = R_PREMIUM_MO.NPAYNUMBE
                                       AND NBILL_ITEM = INSAGL001.NBILL_ITEM_AUX; --COBERTURAS
                                EXCEPTION
                                    WHEN NO_DATA_FOUND THEN
                                       NPREM_RECEIPT := NULL;
                                END;

                                IF (NVL(R_COMMISS_PR.NPROVINCE,0) = 0) THEN
                                    BEGIN
                                        SELECT  A.NPROVINCE
                                          INTO  R_COMMISS_PR.NPROVINCE
                                          FROM  ADDRESS A
                                         WHERE  A.SCLIENT   = INSAGL001.SCLIENT
                                           AND  A.NRECOWNER = 2
                                           AND  A.SRECTYPE = '2'
                                           AND  DEFFECDATE <= R_PREMIUM_MO.DCOMPDATE
                                           AND (DNULLDATE IS NULL
                                            OR  DNULLDATE   < R_PREMIUM_MO.DCOMPDATE); -- PARTICULAR
                                    EXCEPTION
                                        WHEN OTHERS THEN
                                           RAISE;
                                    END;
                                END IF;

                                IF (NTYPE_TRAN = 4) THEN
                                    R_PREMIUM_MO.NPREMIUM   := R_PREMIUM_MO.NPREMIUM * -1;
                                    NPREM_RECEIPT := NPREM_RECEIPT * -1;
                                END IF;

                                CRECOMM_DET (NTYP_ACCO      => NTYP_ACCO,
                                             NIDCONSEC      => NIDCONSEC,
                                             NBRANCH        => R_PREMIUM_MO.NBRANCH,
                                             NPRODUCT       => R_PREMIUM_MO.NPRODUCT,
                                             NCURRENCY      => R_PREMIUM_MO.NCURRENCY,
                                             IN_DOPERDATE   => TRUNC(INSAGL001.DDATE_END_AUX), --to_char(INSAGL001.DCOMPDATE,'DD/MM/YYYY'),
                                             DCOMPDATE      => DTODAY,
                                             STYPE_ACC      => '0',
                                             NRECEIPT       => R_PREMIUM_MO.NRECEIPT,
                                             NACCO_KEY      => NACCOKEY,
                                             NTYPE_TRAN     => NTYPE_TRAN,
                                             NINTERMED      => R_COMMISS_PR.NINTERMED,
                                             SIND_CREDEB    => SDEBITSIDE,
                                             NPROVINCE      => R_COMMISS_PR.NPROVINCE,
                                             NTYP_AMOUNT    => NTYPE_AMOUNT,
                                             SCLIENT        => SCLIENT,
                                             NAMOUNT        => (CASE WHEN R_COMMISS_PR.NADJUSTMENTTYPE_AUX = 3 THEN (NCOMDETAMOUNT_MA * -1) ELSE (CASE WHEN NTYPE_MOVE = 404 THEN (NCOMDETAMOUNT_MA * -1) ELSE NCOMDETAMOUNT_MA END) END ),
                                             NPREMIUM       => (CASE WHEN R_COMMISS_PR.NADJUSTMENTTYPE_AUX = 3 THEN (R_PREMIUM_MO.NPREMIUM * -1) ELSE R_PREMIUM_MO.NPREMIUM END),
                                             NCOLLAMOUNT    => (CASE WHEN R_COMMISS_PR.NADJUSTMENTTYPE_AUX = 3 THEN (R_PREMIUM_MO.NAMOUNT * -1) ELSE R_PREMIUM_MO.NAMOUNT END),
                                             NCOVERBIAMOUNT => (CASE WHEN R_COMMISS_PR.NADJUSTMENTTYPE_AUX = 3 THEN (NPREM_RECEIPT * -1) ELSE NPREM_RECEIPT END),
                                             NUSERCODE      => NUSERCODE,
                                             NCOMMIT        => 2);

                            END;
                            END IF;
                        END;

                    END;
--CRETRACE2('INSAGL001 14', 984, 'FI POR C_TAB_COMPRO ' || TO_CHAR(SYSTIMESTAMP,'dd-mm-yyyy hh24:mi:ss.FF'));
                    END LOOP;

                    IF R_PREMIUM_MO.NTYPE = 321 THEN   -- NTYPE = 321 - PRÉSTAMO DE INTERMEDIARIOS
                    BEGIN



                    FOR R_DAT_LOAN IN C_DAT_LOAN(R_PREMIUM_MO.NPREMIUM,R_COMMISS_PR.NINTERMED ,INSAGL001.DINIT_DATE,INSAGL001.END_DATEAUX   ) LOOP

                        BEGIN
                            BEGIN
                                SELECT NIDCONSEC
                                  INTO NIDCONSEC
                                  FROM MOVE_ACC
                                 WHERE NTYP_ACCO             = INSAGL001.NTYP_ACCO
                                   AND STYPE_ACC             = '0'
                                   AND SCLIENT               = INSAGL001.SCLIENT
                                   AND NCURRENCY             = R_PREMIUM_MO.NCURRENCY
                                   AND NTYPE_MOVE            = 303
                                   AND NINTERMED             = R_COMMISS_PR.NINTERMED
                                   AND NRECEIPT              = R_PREMIUM_MO.NPREMIUM
                                   AND((INSAGL001.SDEBITSIDE = '1'
                                   AND NVL(NCREDIT, 0)       = 0)
                                    OR(INSAGL001.SDEBITSIDE  ='2'
                                   AND NVL(NDEBIT,  0)       = 0))
                                   AND TO_CHAR(DOPERDATE, 'YYYYMMDD') = TO_CHAR(INSAGL001.DDATE_END_AUX,'YYYYMMDD');

                            EXCEPTION
                                WHEN NO_DATA_FOUND THEN
                                    NULL;
                            END;
                            IF NIDCONSEC IS NULL THEN
                            BEGIN
                                NAMODEB  :=  R_DAT_LOAN.NAMOUNT;
                                NAMOCRED :=  0;

                                BEGIN
                                    SELECT SDESCRIPT
                                      INTO SDESCRIPT
                                      FROM TABLE6
                                     WHERE NTYPE_TRAN = 15;
                                EXCEPTION
                                    WHEN NO_DATA_FOUND THEN
                                      SDESCRIPT := NULL;
                                END;

                                CRETMP_AGL001(SKEY          => SKEY,
                                              NTYP_ACCO     => NTYP_ACCO,
                                              STYPE_ACC     => 0,
                                              SCLIENT       => SCLIENT,
                                              NCURRENCY     => R_PREMIUM_MO.NCURRENCY,
                                              DOPERDATE     => R_PREMIUM_MO.DCOMPDATE,--DDATE_END_AUX,,
                                              NINTERMED     => R_COMMISS_PR.NINTERMED,
                                              NTYPE_MOVE    => NTYPE_MOVE,
                                              NAMOUNT       => NCOMDETAMOUNT,
                                              NCREDIT       => NAMOCRED,
                                              NDEBIT        => NAMODEB,
                                              NRECEIPT      => R_PREMIUM_MO.NRECEIPT,
                                              NUSERCODE     => NUSERCODE,
                                              DCOMPDATE     => R_PREMIUM_MO.DCOMPDATE);


                                     INSMOVE_ACC(NTYP_ACCO   => NTYP_ACCO,
                                                            NCURRENCY   => R_PREMIUM_MO.NCURRENCY,                                                    
                                                            SCLIENT     => SCLIENT,
                                                            NTYPE_MOVE  => NTYPE_MOVE, 
                                                            STYPE_ACC   => '0',
                                                            IN_DOPERDATE   => R_PREMIUM_MO.DCOMPDATE,
                                                            NREM_NUMBER   => NIDCONSEC,
                                                            NINTERMED   => R_COMMISS_PR.NINTERMED,
                                                            NAMOUNT     => NCOMDETAMOUNT,
                                                            SDESCRIPT   => SDESCRIPT_MINI,
                                                            NBRANCH     => R_PREMIUM_MO.NBRANCH,
                                                            NPOLICY     => R_PREMIUM_MO.NPOLICY,
                                                            NPRODUCT    => R_PREMIUM_MO.NPRODUCT,
                                                            NTYPE_PAY   => 3,
                                                            NCREDIT     => NAMOCRED, 
                                                            NTYPE_TRAN  => 15,
                                                            NDEBIT      => NAMODEB, 
                                                            NTRANSACTIO =>  R_PREMIUM_MO.NTRANSAC,
                                                            NUSERCODE   => NUSERCODE,
                                                            NPROVINCE   => R_COMMISS_PR.NPROVINCE,
                                                            NRECEIPT    => R_PREMIUM_MO.NRECEIPT,
                                                            NAMOAFECT   => R_COMMISS_PR.NCOM_AFEC, --NCOM_AFEC,
                                                            NAMOEXEN    => R_COMMISS_PR.NCOM_EXEN, --NCOM_EXEN,
                                                            NCOMMIT     => 2,
                                                            NPAYNUMBE   => R_PREMIUM_MO.NPAYNUMBE,
                                                            NCERTIF     => R_PREMIUM_MO.NCERTIF,
                                                            NEXCHANGE   => NEXCHANGE,
                                                            NINTERTYP   => NINTERTYP,
                                                            NCURR_ACC   => NACCOKEY,
                                                            NORIGCURR   => R_PREMIUM_MO.NCURRENCY,
                                                            SCERTYPE     => R_PREMIUM_MO.SCERTYPE,
                                                            NDIGIT          => R_PREMIUM_MO.NDIGIT);
                                                    
                                                    NIDCONSEC := NIDCONSEC + 1;
                                                                
                            END;
                            ELSE
                            BEGIN
                                NAMODEB :=  R_DAT_LOAN.NAMOUNT;
                                NAMOCRED :=  0;

                                UPDATE MOVE_ACC
                                   SET NCREDIT    = NCREDIT + INSAGL001.NAMOCRED,
                                       NDEBIT     = NDEBIT  + INSAGL001.NAMODEB,
                                       NAMOUNT    = NAMOUNT + INSAGL001.NCOMDETAMOUNT,
                                       SSTATREGT  ='1',
                                       NTYPE_PAY  = 3,
                                       NUSERCODE  = INSAGL001.NUSERCODE,
                                       DCOMPDATE  = SYSDATE
                                 WHERE NIDCONSEC  = INSAGL001.NIDCONSEC
                                   AND NTYP_ACCO  = INSAGL001.NTYP_ACCO
                                   AND STYPE_ACC  = '0'
                                   AND NINTERMED  = R_COMMISS_PR.NINTERMED
                                   AND SCLIENT    = INSAGL001.SCLIENT
                                   AND NRECEIPT   = R_PREMIUM_MO.NRECEIPT
                                   AND NCURRENCY  = R_PREMIUM_MO.NCURRENCY
                                   AND TO_CHAR(DOPERDATE, 'YYYYMMDD') = TO_CHAR(INSAGL001.DDATE_END_AUX,'YYYYMMDD')
                                   AND NTYPE_MOVE = 303;

                            END;
                            END IF;

                            UPDATE DAT_LOAN SET SINDICAT  = '1',
                                                NUSERCODE = INSAGL001.NUSERCODE,
                                                DCOMPDATE = INSAGL001.DTODAY
                             WHERE NLOAN     = R_DAT_LOAN.NLOAN
                               AND NRECEIPT  = R_PREMIUM_MO.NRECEIPT;


                        END;
                        END LOOP;
                    END;
                    END IF;

                    NCOMMISSION :=  0;
                    NREST :=  0;

    /*+ SI SE TRATA DE PAGOS O REVERSOS DE PAGO +*/

                    IF R_PREMIUM_MO.NTYPE = 321 OR  -- NTYPE = 34 - LIQUIDACI¿N DEL COBRO
                       R_PREMIUM_MO.NTYPE = 4 THEN
                    BEGIN

    /*+ SE CALCULA EL MONTO DE LA COMISI¿N SEG¿N LA PARTICIPACI¿N DEL INTERMEDIARIO +*/

                        NCOMMISSION :=  R_PREMIUM_MO.NAMOUNT;
                        NCOMMISSION := ABS(NCOMMISSION);
                        NCOMMISSION := NCOMMISSION * (R_COMMISS_PR.NSHARE / 100);

                        NREST       := NCOMMISSION;

            FOR R_LOANS_INT IN C_LOANS_INT(R_COMMISS_PR.NINTERMED) LOOP

                        BEGIN
                            IF NREST > 0 THEN
                            BEGIN
                                NAMOUNT2 :=  NREST * (R_LOANS_INT.NRATE_RET/ 100);

                                INSCALCONVERTEXCHANGE2(PARAMNEXCHANGE => NAUXNULL,
                                                       PARAMNAMOUNT   => NAMOUNT2,
                                                       PARAMNCURORI   => R_PREMIUM_MO.NCURRENCY,
                                                       PARAMNCURDES   => R_LOANS_INT.NCURRENCY,
                                                       PARAMNRESULT   => NAMOUNT2);

                                IF R_PREMIUM_MO.NTYPE IN (3, 4) THEN
                                    NAMOUNT2    :=  NAMOUNT2 * -1;
                                    SDEBITSIDE2 :=  '2';
                                ELSE
                                    SDEBITSIDE2 :=  '1';
                                    IF NAMOUNT2 = R_LOANS_INT.NBALANLOAN THEN
                                        INSCALCONVERTEXCHANGE2(PARAMNEXCHANGE => NAUXNULL,
                                                               PARAMNAMOUNT   => R_LOANS_INT.NBALANLOAN,
                                                               PARAMNCURORI   => R_LOANS_INT.NCURRENCY,
                                                               PARAMNCURDES   => R_PREMIUM_MO.NCURRENCY,
                                                               PARAMNRESULT   => NAMOUNT2);
                                    END IF;
                                END IF;

                                NAUXAMOUNT  := R_LOANS_INT.NBALANLOAN;
                                R_LOANS_INT.NBALANLOAN := R_LOANS_INT.NBALANLOAN - NAMOUNT2;

                                IF R_LOANS_INT.NBALANLOAN <= 0 THEN
                                    NAMOUNT2    :=  NAUXAMOUNT;
                                    R_LOANS_INT.NBALANLOAN :=  0;
                                    R_LOANS_INT.SSTATLOAN   := '4';
                                END IF;

                                NREST := NREST - NAMOUNT2;

                                BEGIN
                                    SELECT NIDCONSEC
                                      INTO NIDCONSEC
                                      FROM MOVE_ACC
                                     WHERE MOVE_ACC.NTYP_ACCO       = INSAGL001.NTYP_ACCO
                                       AND MOVE_ACC.STYPE_ACC       = '0'
                                       AND MOVE_ACC.NINTERMED       = R_COMMISS_PR.NINTERMED
                                       AND MOVE_ACC.NTYPE_MOVE      = 321
                                       AND MOVE_ACC.SCLIENT         = INSAGL001.SCLIENT
                                       AND MOVE_ACC.NCURRENCY       = R_PREMIUM_MO.NCURRENCY
                                       AND((INSAGL001.SDEBITSIDE    ='1'
                                       AND NVL(MOVE_ACC.NCREDIT, 0) = 0)
                                        OR(INSAGL001.SDEBITSIDE     ='2'
                                       AND NVL(MOVE_ACC.NDEBIT,  0) = 0))
                                       AND MOVE_ACC.DOPERDATE       = TO_CHAR(INSAGL001.DDATE_END_AUX, 'YYYYMMDD');
                                EXCEPTION
                                    WHEN NO_DATA_FOUND THEN
                                        NIDCONSEC := NULL;
                                END;

                                IF NIDCONSEC IS NULL THEN
                                BEGIN
                                    SELECT NVL(MAX(NVL(MOVE_ACC.NIDCONSEC,0)), 0)
                                      INTO INSAGL001.NIDCONSEC
                                      FROM MOVE_ACC MOVE_ACC
                                     WHERE MOVE_ACC.NTYP_ACCO       = INSAGL001.NTYP_ACCO
                                       AND MOVE_ACC.STYPE_ACC       ='0'
                                       AND MOVE_ACC.SCLIENT         = INSAGL001.SCLIENT
                                       AND MOVE_ACC.NCURRENCY       = R_PREMIUM_MO.NCURRENCY
                                       AND ((INSAGL001.SDEBITSIDE   ='1'
                                       AND NVL(MOVE_ACC.NCREDIT, 0) = 0)
                                        OR (INSAGL001.SDEBITSIDE    ='2'
                                       AND NVL(MOVE_ACC.NDEBIT , 0) = 0))
                                       AND TO_CHAR(MOVE_ACC.DOPERDATE, 'YYYYMMDD') = TO_CHAR(INSAGL001.DDATE_END_AUX,'YYYYMMDD');

                                    NIDCONSEC :=  NIDCONSEC + 1;

                                    IF NTYPE_TRAN = 321 THEN  -- NTYPE = 321 - PRÉSTAMOS DE INTERMEDIARIOS
                                        NAMODEB  :=  NAMOUNT2;
                                        NAMOCRED :=  0;
                                    ELSE
                                        NAMODEB  :=  0;
                                        NAMOCRED :=  NAMOUNT2;
                                    END IF;

                                    BEGIN
                                        SELECT TABLE6.SDESCRIPT
                                          INTO INSAGL001.SDESCRIPT
                                          FROM TABLE6 TABLE6
                                         WHERE TABLE6.NTYPE_TRAN = 15;
                                    EXCEPTION
                                        WHEN NO_DATA_FOUND THEN
                                          SDESCRIPT :=  NULL;
                                    END;

                                    CRETMP_AGL001(SKEY          => SKEY,
                                                  NTYP_ACCO     => NTYP_ACCO,
                                                  STYPE_ACC     => 0,
                                                  SCLIENT       => SCLIENT,
                                                  NCURRENCY     => R_PREMIUM_MO.NCURRENCY,
                                                  DOPERDATE     => R_PREMIUM_MO.DCOMPDATE,--DDATE_END_AUX,
                                                  NINTERMED     => R_COMMISS_PR.NINTERMED,
                                                  NTYPE_MOVE    => NTYPE_MOVE,
                                                  NAMOUNT       => NCOMDETAMOUNT,
                                                  NCREDIT       => NAMOCRED,
                                                  NDEBIT        => NAMODEB,
                                                  NRECEIPT      => R_PREMIUM_MO.NRECEIPT,
                                                  NUSERCODE     => NUSERCODE,
                                                  DCOMPDATE     => R_PREMIUM_MO.DCOMPDATE);


                                     INSMOVE_ACC(NTYP_ACCO   => NTYP_ACCO,
                                                            NCURRENCY   => R_PREMIUM_MO.NCURRENCY,                                                    
                                                            SCLIENT     => SCLIENT,
                                                            NTYPE_MOVE  => 321, 
                                                            STYPE_ACC   => '0',
                                                            IN_DOPERDATE   => R_PREMIUM_MO.DCOMPDATE,
                                                            NREM_NUMBER   => NIDCONSEC,
                                                            NINTERMED   => R_COMMISS_PR.NINTERMED,
                                                            NAMOUNT     => NAMOUNT2,
                                                            SDESCRIPT   => SDESCRIPT_MINI,
                                                            NBRANCH     => R_PREMIUM_MO.NBRANCH,
                                                            NPOLICY     => R_PREMIUM_MO.NPOLICY,
                                                            NPRODUCT    => R_PREMIUM_MO.NPRODUCT,
                                                            NTYPE_PAY   => 3,
                                                            NCREDIT     => NAMOCRED, 
                                                            NTYPE_TRAN  => 15,
                                                            NDEBIT      => NAMODEB, 
                                                            NTRANSACTIO =>  R_PREMIUM_MO.NTRANSAC,
                                                            NUSERCODE   => NUSERCODE,
                                                            NPROVINCE   => R_COMMISS_PR.NPROVINCE,
                                                            NRECEIPT    => R_PREMIUM_MO.NRECEIPT,
                                                            NAMOAFECT   => R_COMMISS_PR.NCOM_AFEC, --NCOM_AFEC,
                                                            NAMOEXEN    => R_COMMISS_PR.NCOM_EXEN, --NCOM_EXEN,
                                                            NCOMMIT     => 2,
                                                            NPAYNUMBE   => R_PREMIUM_MO.NPAYNUMBE,
                                                            NCERTIF     => R_PREMIUM_MO.NCERTIF,
                                                            NEXCHANGE   => NEXCHANGE,
                                                            NINTERTYP   => NINTERTYP,
                                                            NCURR_ACC   => NACCOKEY,
                                                            NORIGCURR   => R_PREMIUM_MO.NCURRENCY,
                                                            SCERTYPE     => R_PREMIUM_MO.SCERTYPE,
                                                            NDIGIT          => R_PREMIUM_MO.NDIGIT);
                                                    
                                                    NIDCONSEC := NIDCONSEC + 1;

                                END;
                                ELSE
                                    IF NTYPE_TRAN = 321 THEN  -- NTYPE = 321 - LIQUIDACI¿N DEL COBRO
                                        NAMODEB  := NAMOUNT2;
                                        NAMOCRED := 0;
                                    ELSE
                                        NAMODEB  := 0;
                                        NAMOCRED := NAMOUNT2;
                                    END IF;

                                    UPDATE MOVE_ACC SET NCREDIT    = NCREDIT + INSAGL001.NAMOCRED,
                                                        NDEBIT     = NDEBIT  + INSAGL001.NAMODEB ,
                                                        NAMOUNT    = NAMOUNT + INSAGL001.NAMOUNT2,
                                                        SSTATREGT  ='1',
                                                        NTYPE_PAY  = 3,
                                                        NUSERCODE = INSAGL001.NUSERCODE,
                                                        DCOMPDATE = SYSDATE
                                     WHERE NIDCONSEC  = INSAGL001.NIDCONSEC
                                       AND NTYP_ACCO  = INSAGL001.NTYP_ACCO
                                       AND STYPE_ACC  = '0'
                                       AND NINTERMED  = R_COMMISS_PR.NINTERMED
                                       AND SCLIENT    = INSAGL001.SCLIENT
                                       AND NCURRENCY  = R_PREMIUM_MO.NCURRENCY
                                       AND TO_CHAR(DOPERDATE, 'YYYYMMDD') = TO_CHAR(INSAGL001.DDATE_END_AUX,'YYYYMMDD')
                                       AND NTYPE_MOVE = 321;

                                     INSMOVE_ACC (NTYP_ACCO          => NTYP_ACCO,
                                                             STYPE_ACC          => '0',--ok
                                                             NTYPE_MOVE => NTYPE_MOVE,
                                                             SCLIENT            => SCLIENT,
                                                             NCURRENCY          => R_PREMIUM_MO.NCURRENCY,
                                                             NUSERCODE          => NUSERCODE,--ok
                                                             NCOMMIT            => 2,
                                                             NREM_NUMBER => NREM_NUMBER,
                                                            SCERTYPE     => R_PREMIUM_MO.SCERTYPE);      
                                END IF;

                                BEGIN
                                    SELECT NVL(SUM(NVL(NPREMIUM,0)),0)
                                      INTO NPREM_RECEIPT
                                      FROM DETAIL_PRE
                                     WHERE SCERTYPE   = SCERTYPE
                                       AND NBRANCH    = NBRANCH
                                       AND NPRODUCT   = NPRODUCT
                                       AND NRECEIPT   = NRECEIPT
                                       AND NDIGIT     = NDIGIT
                                       AND NPAYNUMBE  = NPAYNUMBE
                                       AND NBILL_ITEM = (SELECT DISTINCT NBILL_ITEM --CONCEPTO DE FACTURACION
                                                           FROM DETAIL_PRE
                                                          WHERE SCERTYPE    = SCERTYPE
                                                            AND NBRANCH     = NBRANCH
                                                            AND NPRODUCT    = NPRODUCT
                                                            AND NRECEIPT    = NRECEIPT
                                                            AND NDIGIT      = NDIGIT
                                                            AND NPAYNUMBE   = NPAYNUMBE
                                                            AND STYPE_DETAI = '1'); --COBERTURAS
                                EXCEPTION
                                    WHEN NO_DATA_FOUND THEN
                                       NPREM_RECEIPT := NULL;
                                END;

                                IF (NTYPE_TRAN = 4) THEN
                                    R_PREMIUM_MO.NPREMIUM   := R_PREMIUM_MO.NPREMIUM * -1;
                                    NPREM_RECEIPT := NPREM_RECEIPT * -1;
                                END IF;

                                CRECOMM_DET(NTYP_ACCO       => NTYP_ACCO,
                                            NIDCONSEC       => NIDCONSEC,
                                            NBRANCH         => R_PREMIUM_MO.NBRANCH,
                                            NPRODUCT        => R_PREMIUM_MO.NPRODUCT,
                                            NCURRENCY       => R_PREMIUM_MO.NCURRENCY,
                                            IN_DOPERDATE    => R_PREMIUM_MO.DCOMPDATE,--TRUNC(INSAGL001.DDATE_END_AUX),
                                            DCOMPDATE       => DTODAY,
                                            STYPE_ACC       => '0',
                                            NRECEIPT        => R_PREMIUM_MO.NRECEIPT,
                                            NACCO_KEY       => NACCOKEY,
                                            NTYPE_TRAN      => 15,
                                            NINTERMED       => R_COMMISS_PR.NINTERMED,
                                            SIND_CREDEB     => SDEBITSIDE2,
                                            NPROVINCE       => R_COMMISS_PR.NPROVINCE,
                                            NTYP_AMOUNT     => NTYPE_AMOUNT,
                                            SCLIENT         => SCLIENT,
                                            NAMOUNT         => (CASE WHEN NTYPE_MOVE = 404 THEN (NCOMDETAMOUNT_MA * -1) ELSE NCOMDETAMOUNT_MA END),
                                            NPREMIUM        => R_PREMIUM_MO.NPREMIUM,
                                            NCOLLAMOUNT     => R_PREMIUM_MO.NAMOUNT,
                                            NCOVERBIAMOUNT  => NPREM_RECEIPT,
                                            NUSERCODE       => NUSERCODE,
                                            NCOMMIT         => 2);

                                UPDATE LOANS_INT SET NBALANLOAN  = R_LOANS_INT.NBALANLOAN,
                                                     NAMOLASTPAY = INSAGL001.NAMOUNT2   ,
                                                     DDATE_PAY   = R_PREMIUM_MO.DCOMPDATE  ,
                                                     DCOMPDATE   = INSAGL001.DTODAY     ,
                                                     NUSERCODE   = INSAGL001.NUSERCODE  ,
                                                     SSTATLOAN   = R_LOANS_INT.SSTATLOAN
                               WHERE ROWID = R_LOANS_INT.NROW;

                            END;
                            END IF;

                        END;
                        END LOOP;
                    END;
                    END IF;

    /*+  SE ACTUALIZAN LOS REGISTROS, PARA RECIBOS QUE NO EST¿N FINANCIADOS, EN LA TABLA DE MOVIMIENTOS DE UN RECIBO (PREMIUM_MO) Y EN LA TABLA DE HISTORIA DE LOS GIROS (DRAFT_HIST), +*/
    /*+ PARA RECIBOS FINANCIADOS, CON EL ESTADO DE ¿PROCESADO POR LA PREPARACI¿N DE CUENTA CORRIENTE DE INTERMEDIARIOS¿. +*/

                    IF NVL(R_PREMIUM_MO.NCONTRAT,0) > 0 THEN
                        UPDATE DRAFT_HIST SET SINTERMEI = '1',
                                                              NUSERCODE = INSAGL001.NUSERCODE,
                                                              DCOMPDATE = INSAGL001.DTODAY
                         WHERE NTRANSAC  = R_PREMIUM_MO.NTRANSAC
                              AND NCONTRAT  = R_PREMIUM_MO.NCONTRAT
                              AND NDRAFT    = R_PREMIUM_MO.NDRAFT;
                    ELSE
                        UPDATE PREMIUM_MO SET SINTERMEI = '1',
                                                                NUSERCODE = INSAGL001.NUSERCODE,
                                                                DCOMPDATE = INSAGL001.DTODAY
                         WHERE NRECEIPT  = R_PREMIUM_MO.NRECEIPT
                           AND SCERTYPE  = R_PREMIUM_MO.SCERTYPE
                           AND NDIGIT    = R_PREMIUM_MO.NDIGIT
                           AND NTRANSAC  = R_PREMIUM_MO.NTRANSAC;
                    END IF;


                    SELECT COUNT(NCURRENCY)
                       INTO NCOUNT
                      FROM INTERM_BUD
                    WHERE NINTERMED = NINTERMED;

                    IF NVL(NCOUNT,0) <> 0 THEN
                        NCOMMISSION :=  R_PREMIUM_MO.NPREMIUM * (R_COMMISS_PR.NSHARE / 100);
                        IF R_PREMIUM_MO.NTYPE <> 1 THEN
                            INSERT INTO AGL001_TEMP_TINTERM_TMP(SKEY     , NRECEIPT , NCODE    ,
                                                                NBRANCH  , NPRODUCT , NCURRENCY,
                                                                DOPERDATE, NTRATYPEI, NTYPE    ,
                                                                NAMOUNT)
                                                        VALUES (INSAGL001.SKEY     , R_PREMIUM_MO.NRECEIPT ,R_COMMISS_PR.NINTERMED,
                                                                R_PREMIUM_MO.NBRANCH  , R_PREMIUM_MO.NPRODUCT , R_PREMIUM_MO.NCURRENCY,
                                                                R_PREMIUM_MO.DCOMPDATE, R_PREMIUM_MO.NTRATYPEI, R_PREMIUM_MO.NTYPE    ,
                                                                INSAGL001.NCOMMISSION);
                        END IF;
                    END IF;

                END;
                --CRETRACE2('INSAGL001 15', 984, 'FIN POR C_COMMISS_PR ' || TO_CHAR(SYSTIMESTAMP,'dd-mm-yyyy hh24:mi:ss.FF'));
                END LOOP;
            END;

        END;

--CRETRACE2('INSAGL001 16', 984, 'FIN C_PREMIUM_MO ' || R_PREMIUM_MO.NRECEIPT || 'hora |' || TO_CHAR(SYSTIMESTAMP,'dd-mm-yyyy hh24:mi:ss.FF'));
        END LOOP;

        /* MANEJO PARA MOVIMIENTOS MANUALES DF-004*/
        OPEN C_MAN_MOV_ACC;
       FETCH C_MAN_MOV_ACC
        INTO R_MAN_MOV_ACC;

        WHILE C_MAN_MOV_ACC%FOUND LOOP
        BEGIN
--CRETRACE2('INSAGL001 17', 984, 'INICIO C_MAN_MOV_ACC ' || TO_CHAR(SYSTIMESTAMP,'dd-mm-yyyy hh24:mi:ss.FF'));
            BEGIN
                SELECT NVL(MAX(PMO.DSTATDATE),R_MAN_MOV_ACC.DOPERDATE)
                  INTO PMO_DCOMPDATE
                  FROM PREMIUM_MO PMO
                 WHERE PMO.NRECEIPT  IN (CASE WHEN NVL(R_MAN_MOV_ACC.NRECEIPT,0) = 0 THEN (SELECT NRECEIPT FROM PREMIUM WHERE  NPOLICY = R_MAN_MOV_ACC.NPOLICY) ELSE R_MAN_MOV_ACC.NRECEIPT END)
                   AND (PMO.SINTERMEI  = '2'
                    OR PMO.SINTERMEI IS NULL);
            EXCEPTION
            WHEN NO_DATA_FOUND THEN
--             RAISE;
                PMO_DCOMPDATE := R_MAN_MOV_ACC.DOPERDATE;
            WHEN OTHERS THEN
--             RAISE;
                PMO_DCOMPDATE := R_MAN_MOV_ACC.DOPERDATE;
            END;

            BEGIN
                CRETMP_AGL001(SKEY          => INSAGL001.SKEY,
                              NTYP_ACCO     => R_MAN_MOV_ACC.NTYP_ACCO,
                              STYPE_ACC     => R_MAN_MOV_ACC.STYPE_ACC,
                              SCLIENT       => R_MAN_MOV_ACC.SCLIENT,
                              NCURRENCY     => R_MAN_MOV_ACC.NCURRENCY,
                              DOPERDATE     => PMO_DCOMPDATE,--DDATE_END_AUX,
                              NINTERMED     => R_MAN_MOV_ACC.NINTERMED,
                              NTYPE_MOVE    => R_MAN_MOV_ACC.NTYPE_MOVE,
                              NAMOUNT       => R_MAN_MOV_ACC.NAMOUNT,
                              NCREDIT       => R_MAN_MOV_ACC.NCREDIT,
                              NDEBIT        => R_MAN_MOV_ACC.NDEBIT,
                              NRECEIPT      => R_MAN_MOV_ACC.NRECEIPT,
                              NUSERCODE     => INSAGL001.NUSERCODE,
                              DCOMPDATE     => SYSDATE);
            EXCEPTION
                WHEN OTHERS THEN
                    RAISE;
            END;

            IF R_MAN_MOV_ACC.SSOCIALRET = '1' THEN
--CRETRACE2('INSAGL001 18', 984, 'INICIO R_MAN_MOV_ACC INSCALISSS ' || TO_CHAR(SYSTIMESTAMP,'dd-mm-yyyy hh24:mi:ss.FF'));
                INSCALISSS(NTYP_ACCO    => R_MAN_MOV_ACC.NTYP_ACCO,
                           STYPE_ACC    => R_MAN_MOV_ACC.STYPE_ACC,
                           SCLIENT      => R_MAN_MOV_ACC.SCLIENT,
                           NCURRENCY    => R_MAN_MOV_ACC.NCURRENCY,
                           DOPERDATE    => PMO_DCOMPDATE,--DDATE_END_AUX,
                           NIDCONSEC    => R_MAN_MOV_ACC.NIDCONSEC,
                           NACCO_KEY    => NACCOKEY,
                           NINTERTYP    => R_MAN_MOV_ACC.NINTERTYP,
                           NINTERMED    => R_MAN_MOV_ACC.NINTERMED,
                           NPROVINCE    => R_MAN_MOV_ACC.NPROVINCE,
                           NCREDIT      => R_MAN_MOV_ACC.NCREDIT,
                           NDEBIT       => R_MAN_MOV_ACC.NDEBIT,
                           NUSERCODE    => INSAGL001.NUSERCODE,
                           SCERTYPE     => R_MAN_MOV_ACC.SCERTYPE,
                           NBRANCH      => R_MAN_MOV_ACC.NBRANCH,
                           NPRODUCT     => R_MAN_MOV_ACC.NPRODUCT,
                           NRECEIPT     => R_MAN_MOV_ACC.NRECEIPT,
                           NDIGIT       => R_MAN_MOV_ACC.NDIGIT,
                           NPAYNUMBE    => R_MAN_MOV_ACC.NPAYNUMBE,
                           SKEY         => INSAGL001.SKEY,
                           NEXCHANGE    => R_MAN_MOV_ACC.NEXCHANGE,
                           NORIGCURR    => R_MAN_MOV_ACC.NCURRENCY,
                           NCOM_AFEC    => R_MAN_MOV_ACC.NAMOAFECT,
                           NCOM_EXEN    => R_MAN_MOV_ACC.NAMOEXEN,
                           NCOM_NOCLAS  => R_MAN_MOV_ACC.NAMONOCLAS,
                           NAMOUNT_MA   => NAMOUNT_RET,
                           NCREDIT_MA   => NCREDIT_RET,
                           NDEBIT_MA    => NDEBIT_RET,
                           NID_MOVE_ACC => NID_MOVE_ACC,
                           SDEBIT_SIDE_RET => (CASE WHEN R_MAN_MOV_ACC.NTYPE_MOVE = 404 THEN '2' ELSE '1' END),
                           NCOMIDCONSEC => NAUXNULL,
                           NADJUSTMENTTYPE => NADJUSTMENTTYPE_AUX);

                 UPDATE MOVE_ACC SET   MOVE_ACC.NCURR_ACC = NACCOKEY,
                                       MOVE_ACC.NTRANSACTIO = NACCOKEY,
                                       MOVE_ACC.DCOMPDATE = SYSDATE,
                                       MOVE_ACC.NUSERCODE = INSAGL001.NUSERCODE
                                WHERE  NTYP_ACCO  = R_MAN_MOV_ACC.NTYP_ACCO
                                 AND   STYPE_ACC  = R_MAN_MOV_ACC.STYPE_ACC
                                 AND   SCLIENT    = R_MAN_MOV_ACC.SCLIENT
                                 AND   NCURRENCY  = R_MAN_MOV_ACC.NCURRENCY
                                 AND   DOPERDATE <= INSAGL001.END_DATEAUX
                                 AND   NIDCONSEC  = R_MAN_MOV_ACC.NIDCONSEC;
--CRETRACE2('INSAGL001 19', 984, 'FIN R_MAN_MOV_ACC INSCALISSS ' || TO_CHAR(SYSTIMESTAMP,'dd-mm-yyyy hh24:mi:ss.FF'));

            END IF;

            IF R_MAN_MOV_ACC.SIIBBRET = '1' THEN
--CRETRACE2('INSAGL001 20', 984, 'INICIO R_MAN_MOV_ACC INSCALRETIIBB_RG ' || TO_CHAR(SYSTIMESTAMP,'dd-mm-yyyy hh24:mi:ss.FF'));
                        INSCALRETIIBB_RG(NTYP_ACCO                 =>  R_MAN_MOV_ACC.NTYP_ACCO,
                                          STYPE_ACC                 => R_MAN_MOV_ACC.STYPE_ACC,
                                          DOPERDATE                 => PMO_DCOMPDATE,--DDATE_END_AUX,
                                          NACCO_KEY                 => NACCOKEY,
                                          SCLIENT                   => R_MAN_MOV_ACC.SCLIENT,
                                          NAMOUNT                   => R_MAN_MOV_ACC.NAMOUNT,
                                          IIBBAMOUNT                => INSAGL001.IIBBAMOUNT,
                                          NVATAFFECTYPE             => R_MAN_MOV_ACC.NVATAFFECTYPE,
                                          NCURRENCY                 => R_MAN_MOV_ACC.NCURRENCY,
                                          NID_MOVE_ACC              => NID_MOVE_ACC,
                                          SDEBIT_SIDE_RET           => (CASE WHEN R_MAN_MOV_ACC.NTYPE_MOVE = 404 THEN '2' ELSE '1' END),
                                          SKEY                      => INSAGL001.SKEY,
                                          NUSERCODE                 => INSAGL001.NUSERCODE,
                                          NEXCHANGE                 => R_MAN_MOV_ACC.NEXCHANGE,
                                          NINTERTYP                 => R_MAN_MOV_ACC.NINTERTYP,
                                          NINTERMED                 => R_MAN_MOV_ACC.NINTERMED,
                                          NBRANCH                   => R_MAN_MOV_ACC.NBRANCH,
                                          NPRODUCT                  => R_MAN_MOV_ACC.NPRODUCT,
                                          SCERTYPE                  => R_MAN_MOV_ACC.SCERTYPE,
                                          NADJUSTMENTTYPE           => NADJUSTMENTTYPE_AUX);

                     UPDATE MOVE_ACC SET   MOVE_ACC.NCURR_ACC = NACCOKEY,
                                           MOVE_ACC.NTRANSACTIO = NACCOKEY,
                                           MOVE_ACC.DCOMPDATE = SYSDATE,
                                           MOVE_ACC.NUSERCODE = INSAGL001.NUSERCODE
                                    WHERE  NTYP_ACCO  = R_MAN_MOV_ACC.NTYP_ACCO
                                     AND   STYPE_ACC  = R_MAN_MOV_ACC.STYPE_ACC
                                     AND   SCLIENT    = R_MAN_MOV_ACC.SCLIENT
                                     AND   NCURRENCY  = R_MAN_MOV_ACC.NCURRENCY
                                     AND   DOPERDATE <= INSAGL001.END_DATEAUX
                                     AND   NIDCONSEC  = R_MAN_MOV_ACC.NIDCONSEC;
--CRETRACE2('INSAGL001 21', 984, 'FIN R_MAN_MOV_ACC INSCALRETIIBB_RG ' || TO_CHAR(SYSTIMESTAMP,'dd-mm-yyyy hh24:mi:ss.FF'));
            END IF;
        END;

        FETCH C_MAN_MOV_ACC
         INTO R_MAN_MOV_ACC;
        END LOOP;
        CLOSE C_MAN_MOV_ACC;

--CRETRACE2('INSAGL001 22', 984, 'INICIO GANANCIAS ' || TO_CHAR(SYSTIMESTAMP,'dd-mm-yyyy hh24:mi:ss.FF'));
    /*+ REALIZA EL C¿LCULO DE LA RETENCI¿N POR GANANCIAS, HACIENDO EL LLAMADO A LA RUTINA DE RETENCI¿N DE GANANCIAS. +*/
        INSCALEARNINGS (NTYP_ACCO    => 1, --Clientes
                        STYPE_ACC    => '0',
                        DOPERDATE    => TRUNC(DDATE_END_AUX),
                        NACCO_KEY    => NACCOKEY,
                        SKEY         => SKEY,
                        NUSERCODE    => NUSERCODE);

--CRETRACE2('INSAGL001 23', 984, 'FIN GANANCIAS ' || TO_CHAR(SYSTIMESTAMP,'dd-mm-yyyy hh24:mi:ss.FF'));

        INSERT INTO AGL001_TEMP_TINTERCODE_TMP(SKEY, NCODE)
             SELECT DISTINCT INSAGL001.SKEY, NCODE
               FROM AGL001_TEMP_TINTERM_TMP
              WHERE SKEY = INSAGL001.SKEY;

FOR R_INTERMEDIA IN C_INTERMEDIA LOOP
--CRETRACE2('INSAGL001 24', 984, 'INICIO C_INTERMEDIA ' || TO_CHAR(SYSTIMESTAMP,'dd-mm-yyyy hh24:mi:ss.FF'));

        BEGIN
            IF R_INTERMEDIA.NREAL_TOTAL IS NULL THEN
                R_INTERMEDIA.NREAL_TOTAL :=  0;
            END IF;
            SSELECT := 'SELECT NVL(SUM(NAMOUNT),0) '
                    || ' FROM AGL001_TEMP_TINTERM_TMP '
                    || ' WHERE NCODE = ' || TO_CHAR(R_INTERMEDIA.NCODE)
                    || ' AND NBRANCH = ' || TO_CHAR(R_INTERMEDIA.NBRANCH);

            IF NVL(R_INTERMEDIA.NCURRENCY,0) = 0 THEN
                SSELECT := SSELECT || ' AND NCURRENCY > 0';
            ELSE
                SSELECT := SSELECT || ' AND NCURRENCY = ' || TO_CHAR(R_INTERMEDIA.NCURRENCY);
            END IF;

            IF NVL(R_INTERMEDIA.NPRODUCT,0) = 0 THEN
                SSELECT :=  SSELECT || ' AND NPRODUCT > 0';

            ELSE
                SSELECT :=  SSELECT || ' AND NPRODUCT = ' || TO_CHAR(R_INTERMEDIA.NPRODUCT);
            END IF;

            IF R_INTERMEDIA.STYPE_INFOR = '1'       THEN
                SSELECT :=  SSELECT || ' AND NTRATYPEI = ''1'' AND NTYPE = ''1''';

            ELSIF R_INTERMEDIA.STYPE_INFOR = '3' THEN
                SSELECT :=  SSELECT || ' AND NTRATYPEI = ''2'' AND NTYPE = ''1''';

            ELSE
                SSELECT :=  SSELECT || ' AND NTRATYPEI > 0 AND ((NTYPE > 1 AND NTYPE < 10) OR NTYPE > 11)';
            END IF;

            IF R_INTERMEDIA.SPERIODTYP = '4' THEN
                NPERIOD_TYP2 :=  6;
            ELSE
                NPERIOD_TYP2 :=  TO_NUMBER(R_INTERMEDIA.SPERIODTYP);
            END IF;

            IF NPERIOD_TYP2 = 5 THEN
                SINIT_DATE   :=  '01/01/' || TO_CHAR(R_INTERMEDIA.NYEAR);
                SEND_DATE    :=  '30/12/' || TO_CHAR(R_INTERMEDIA.NYEAR);
                IN_END_DATE  :=  TO_DATE(SEND_DATE , 'DD/MM/YYYY');
                INIT_DATEAUX :=  TO_DATE(SINIT_DATE, 'DD/MM/YYYY');
            ELSE
                SEND_MONTH      :=  TO_CHAR((NPERIOD_TYP2 * R_INTERMEDIA.NPERIODNUM));
                SINIT_MONTH     :=  TO_CHAR(TO_NUMBER(SEND_MONTH) - (NPERIOD_TYP2));
                IF SINIT_MONTH   = '0' THEN
                    SINIT_MONTH := '1';
                END IF;
                IF TO_NUMBER(SEND_MONTH) < 10 THEN
                    SEND_MONTH  := '0' || RTRIM(SEND_MONTH);
                END IF;

                IF TO_NUMBER(SINIT_MONTH) < 10 THEN
                    SINIT_MONTH := '0' || RTRIM(SINIT_MONTH);
                END IF;

                IF SEND_MONTH    = '02' THEN
                    SEND_DATE   := '28/02/' || TO_CHAR(R_INTERMEDIA.NYEAR);

                ELSIF SEND_MONTH = '04' OR
                      SEND_MONTH = '06' OR
                      SEND_MONTH = '09' OR
                      SEND_MONTH = '11' THEN
                      SEND_DATE :=  '30/' || SEND_MONTH || '/' || TO_CHAR(R_INTERMEDIA.NYEAR);

                ELSIF SEND_MONTH = '01' OR
                      SEND_MONTH = '03' OR
                      SEND_MONTH = '05' OR
                      SEND_MONTH = '10' OR
                      SEND_MONTH = '12' THEN

                    SEND_DATE   := '31/' || SEND_MONTH || '/' || TO_CHAR(R_INTERMEDIA.NYEAR);
                END IF;
                SINIT_DATE      := '01/' || SINIT_MONTH || '/' || TO_CHAR(R_INTERMEDIA.NYEAR);
                IN_END_DATE     := TO_DATE(SEND_DATE , 'DD/MM/YYYY');
                INIT_DATEAUX    := TO_DATE(SINIT_DATE, 'DD/MM/YYYY');
            END IF;
            SSELECT :=  SSELECT || ' AND TO_CHAR(DOPERDATE,''YYYYMMDD'') >= ' || '''' || TO_CHAR(DINIT_DATE, 'YYYYMMDD') || '''';
            SSELECT :=  SSELECT || ' AND TO_CHAR(DOPERDATE,''YYYYMMDD'') <= ' || '''' || TO_CHAR(DEND_DATE, 'YYYYMMDD') || '''';
            C_COMM_DET_S := DBMS_SQL.OPEN_CURSOR;
            DBMS_SQL.PARSE(C_COMM_DET_S,SSELECT,DBMS_SQL.NATIVE );
            DBMS_SQL.DEFINE_COLUMN(C_COMM_DET_S,1,NSUM);
            C_COMM_DET_S_REG := DBMS_SQL.EXECUTE(C_COMM_DET_S);
            C_COMM_DET_S_REG := DBMS_SQL.FETCH_ROWS(C_COMM_DET_S);
            DBMS_SQL.COLUMN_VALUE(C_COMM_DET_S,1,NSUM);

            IF (NSUM IS NOT NULL) THEN
                IF R_INTERMEDIA.STYPE_INFOR = '2' THEN
                    NSUM :=  NSUM * (-1);
                END IF;

                R_INTERMEDIA.NREAL_TOTAL :=  R_INTERMEDIA.NREAL_TOTAL + NSUM;

                IF R_INTERMEDIA.NBRANCH IS NULL THEN
                    UPDATE INTERM_BUD
                       SET NREAL_TOTAL = R_INTERMEDIA.NREAL_TOTAL,
                           DCOMPDATE   = SYSDATE,
                           NUSERCODE   = INSAGL001.NUSERCODE
                     WHERE NINTERMED   = R_INTERMEDIA.NCODE
                       AND NBRANCH     = R_INTERMEDIA.NBRANCH
                       AND NCURRENCY   = R_INTERMEDIA.NCURRENCY
                       AND DEFFECDATE <= INSAGL001.DTODAY
                       AND DNULLDATE IS NULL;

                ELSIF R_INTERMEDIA.NPRODUCT IS NULL THEN
                    UPDATE INTERM_BUD
                       SET NREAL_TOTAL = R_INTERMEDIA.NREAL_TOTAL,
                           DCOMPDATE   = SYSDATE,
                           NUSERCODE   = INSAGL001.NUSERCODE
                     WHERE NINTERMED   = R_INTERMEDIA.NCODE
                       AND NCURRENCY   = R_INTERMEDIA.NCURRENCY
                       AND NBRANCH     = R_INTERMEDIA.NBRANCH
                       AND DEFFECDATE <= INSAGL001.DTODAY
                       AND DNULLDATE IS NULL;

                ELSIF R_INTERMEDIA.NPRODUCT IS NOT NULL THEN
                    UPDATE INTERM_BUD
                       SET NREAL_TOTAL = R_INTERMEDIA.NREAL_TOTAL,
                           DCOMPDATE   = SYSDATE,
                           NUSERCODE   = INSAGL001.NUSERCODE
                     WHERE NINTERMED  = R_INTERMEDIA.NCODE
                       AND NCURRENCY  = R_INTERMEDIA.NCURRENCY
                       AND NBRANCH    = R_INTERMEDIA.NBRANCH
                       AND NPRODUCT   = R_INTERMEDIA.NPRODUCT
                       AND DEFFECDATE<= INSAGL001.DTODAY
                       AND DNULLDATE IS NULL;
                END IF;
            END IF;

            DBMS_SQL.CLOSE_CURSOR(C_COMM_DET_S);

        END;
--CRETRACE2('INSAGL001 25', 984, 'FIN C_INTERMEDIA ' || TO_CHAR(SYSTIMESTAMP,'dd-mm-yyyy hh24:mi:ss.FF'));
        END LOOP;


        IF SPRELIMINARY != '1' THEN
        BEGIN
            BEGIN
                SELECT 1
                  INTO STOO_SELCNT
                  FROM DUAL
                 WHERE EXISTS (SELECT 1
                                 FROM CTROL_DATE CTROL_DATE
                                WHERE NTYPE_PROCE = 9);
            EXCEPTION
                WHEN NO_DATA_FOUND THEN
                    STOO_SELCNT := 0;
            END;

            IF STOO_SELCNT <> 0 THEN
                UPDCTROL_DATE(9, DEND_DATE, NUSERCODE);
            ELSE
                CRECTROL_DATE(9, DEND_DATE, NUSERCODE);
            END IF;

            COMMIT;
        END;
        ELSE
        BEGIN
            ROLLBACK;
        END;
        END IF;

        VTBATCHPKG.ENDBATCH(SKEY, VTBATCHPKG.NSTAT_OK);
    ELSE
            UPDATE BATCH_JOB SET NSTATUS   = VTBATCHPKG.NSTAT_ERR,
                                 DEND      = SYSDATE,
                                 DCOMPDATE = SYSDATE
            WHERE SKEY = SKEY;
    COMMIT;
    END IF;
EXCEPTION
    WHEN OTHERS THEN
    CRETRACE2('INSAGL001',984, ' ERROR: ' || sys.DBMS_UTILITY.format_error_stack ||
              ' BACKTRACE: ' || sys.DBMS_UTILITY.format_error_backtrace ||
              ' FORMAT_CALL_STACK: ' || sys.DBMS_UTILITY.format_call_stack);

        STOO_ERROR  := SQLCODE;
        STOO_ERRMSG := SQLERRM;

        ROLLBACK;

        VTBATCHPKG.ENDBATCH(SKEY, VTBATCHPKG.NSTAT_ERR, INSAGL001.NUSERCODE);

END INSAGL001;

PROCEDURE REAAGL001
/*--------------------------------------------------------------------------------------*/
/* NOMBRE    : REAAGL001                                                                */
/* OBJETIVO  : REALIZAR LA PREPARACI¿N CUENTAS CORRIENTES DE INTERMEDIARIOS             */
/* PARAMETROS: 1 - INIT_DATE  : FECHA INICIAL                                           */
/*             2 - END_DATE   : FECHA FINAL                                             */
/*             3 - NUSERCODE  : CODIGO DEL USUARIO                                      */
/*                                                                                      */
/* SOURCESAFE INFORMATION:                                                              */
/*     $Author: cmachado $                                                             */
/*     $Date: 2016-06-24 12:22:30 -0300 (Fri, 24 Jun 2016) $                                                          */
/*     $Revision: 29806 $                                                                   */
/*--------------------------------------------------------------------------------------*/
    (SKEY               TMP_AGL001.SKEY%TYPE,
     DINIT_DATE         PREMIUM_MO.DCOMPDATE%TYPE,
     DEND_DATE          PREMIUM_MO.DCOMPDATE%TYPE,
     NUSERCODE          PREMIUM_MO.NUSERCODE%TYPE,
     SPRELIMINARY       PREMIUM_MO.SCERTYPE%TYPE DEFAULT '1',
     RC1                IN OUT INSAGL001PKG.RCT1) AS


BEGIN
        OPEN RC1 FOR
            SELECT AGL001.NTYP_ACCO,                AGL001.STYPE_ACC,               AGL001.SCLIENT,
                   C.SCLIENAME,                     AGL001.NCURRENCY,               T11.SDESCRIPT AS SCURRENCY,
                   AGL001.DOPERDATE,                AGL001.NIDCONSEC,               AGL001.NINTERMED,
                   AGL001.NCREDIT,                  AGL001.NDEBIT,                  AGL001.NAMOUNT,
                   AGL001.NRECEIPT,                 AGL001.NTYPE_MOVE,              T401.SDESCRIPT AS STYPE_MOVE
              FROM TMP_AGL001 AGL001
              JOIN TABLE11 T11
                ON T11.NCODIGINT    = AGL001.NCURRENCY
              JOIN INTERMEDIA I
                ON I.NINTERMED      = AGL001.NINTERMED
              JOIN CLIENT C
                ON C.SCLIENT        = AGL001.SCLIENT
              JOIN TABLE401 T401
                ON T401.NTYPE_MOVE  = AGL001.NTYPE_MOVE
             --WHERE SKEY             = SKEY;
             WHERE SKEY             = REAAGL001.SKEY
               AND (NVL(AGL001.NCREDIT,0) <> 0
                OR  NVL(AGL001.NDEBIT,0) <> 0)
              ORDER BY I.NINTERMED, AGL001.NIDCONSEC;

END REAAGL001;
END INSAGL001PKG;
/