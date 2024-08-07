CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       $Woods Hole Oceanographic Institution   source        
Argo float     history       92020-04-11T22:00:45Z creation; 2022-03-01T20:08:48Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         iPRIMARY | https://orcid.org/0000-0001-5113-1068 | Deborah West-Mack, Woods Hole Oceanographic Institution         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7d   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7t   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7x   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7|   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  7�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  8<   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  8�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        9   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    9$   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    9(   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  9,   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    9l   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    9t   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  9x   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  9�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  9�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    :8   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           :@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    :P   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            :T   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           :d   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           :t   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    :�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    :�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    :�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        <�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    <�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    <�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    <�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  <�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \D   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  d,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �D   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �D   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 9t   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � A\   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � `�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � h�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �(   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �0   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �8   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �@   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �H   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �$Argo profile    3.1 1.2 19500101000000  20200411220045  20220301150848  4903224 4903224 US ARGO PROJECT                                                 US ARGO PROJECT                                                 WHOI: WIJFFELS, JAYNE, ROBBINS                                  WHOI: WIJFFELS, JAYNE, ROBBINS                                  PRES            TEMP            PSAL            PRES            TEMP            PSAL                  AA  AOAO7999                            7999                            2C  2C  DD  S2A                             S2A                             7494                            7494                            SBE602 15Aug17 ARM V2.4         SBE602 15Aug17 ARM V2.4         854 854 @���X�v@���X�v11  @����
@@����
@@-�a��@-�a���LT����LT���11  GPS     GPS     Primary sampling: averaged [nominal 2 dbar binned data sampled at 0.5 Hz from a SBE41CP]                                                                                                                                                                        Near-surface sampling: discrete, pumped [data sampled at 1.0Hz from the same SBE41CP]                                                                                                                                                                                 AA  AA  AA  ?�  @   @B�\@�  @��R@�  @�\A ��A  A#33A?\)A`��A�  A��A��A��A��A�Q�A�Q�A�Q�B (�B�
B�
B  B   B(  B0  B7�
B@  BH  BP  BX  B_�
Bh  Bp  Bw�B�
B��B�  B�{B�  B��B��B��B�  B�  B�  B�  B��B��B�  B��B�  B�{B�{B��B��B�  B�  B�  B��B��B��B��B��B�  B��B��C   C��C  C  C��C	��C��C  C  C��C  C��C��C  C
=C  C��C!�C$  C&  C'��C)��C,
=C.  C/��C1��C4  C6  C8
=C:
=C<
=C>  C?��CB  CD  CF  CH  CJ  CL  CM��CO��CQ��CT  CV
=CX  CY��C\  C^
=C`
=Cb  Cd  Ce��Cg��Cj  Cl  Cn
=Cp
=Cr
=Ct
=Cv
=Cw��Cy��C|  C~  C�  C���C���C�  C���C���C�  C�  C���C���C�  C�  C���C�  C���C�  C�C�  C���C���C�  C�C�  C���C�  C�  C�  C�C�C�C�C���C���C�  C�  C�  C�  C�  C���C���C�  C�  C�  C�  C�  C���C�  C�  C�C�  C�  C�C�
=C�  C�  C�  C�  C���C���C�  C�C�C�  C�  C�C�C�  C���C���C���C���C���C�  C�C�  C�  C�  C�  C�  C�  C�  C�  C�  C���C���C���C���C���C�  C�  C���C�  C�C�  C���C���C�C�  C���C���C�  C���C���C���C���C�  C���C���C�  C�  C���C�  C�  C�  C���C�  C�  C�  C�  C�  C���C���C�  C�C�C�C���C���D   D � D�D��D�qD}qD�qD}qD  D� D�qD��D�D� D�D� D�qD}qD�qD	}qD	�qD
z�D
��D}qD  D� D  D� D  D}qD  D��D�D� D  D� D�qD}qD�qD}qD  D��D  D��D�D��D�D� D  D� D  D� D  D� D�qD}qD�qD}qD  D}qD  D� D�D� D   D }qD �qD!� D"�D"� D"�qD#� D#�qD$}qD%  D%}qD%�qD&� D'�D'� D'�qD(� D)�D)��D*  D*� D+�D+� D,  D,� D-  D-��D.  D.}qD.�qD/}qD0  D0��D1  D1� D2  D2� D3�D3��D4  D4� D5  D5}qD5�qD6� D7  D7� D8�D8� D9  D9� D9�qD:}qD;  D;� D<  D<� D=  D=� D>�D>��D?�D?� D@  D@� D@�qDA}qDA�qDB� DC  DC}qDC�qDD� DE�DE� DE��DF� DG�DG� DH  DH� DI  DI��DJ�DJ� DK  DK��DL  DL� DM  DM}qDM�qDN}qDO  DO��DP  DP}qDP�qDQ}qDQ�qDR� DS  DS��DT�DT� DU  DU� DU�qDV��DW�DW��DXDX��DY�DY� DY�qDZ� D[�D[� D\  D\}qD]  D]� D]�qD^� D_  D_� D`  D`��Da�Da� Db  Db� Db�qDc� Dd  Dd� Dd�qDe� Df  Df� Dg  Dg� Dg�qDh}qDh�qDi� Di�qDj}qDk  Dk}qDk�qDl}qDm  Dm� Dn  Dn� Do  Do��Dp  Dp}qDq  Dq� Dr  Dr� Ds  Ds� Dt  Dt��Du�Du��Dv  Dv}qDv�qDw}qDx  Dx}qDx�qDy}qDy�qDz� D{  D{� D|�D|��D|�qD}}qD}�qD~}qD~�qD}qD�qD�@ D��HD�� D���D�@ D�� D�� D�  D�>�D�~�D��qD�  D�AHD�� D�� D�HD�@ D�� D��HD�  D�>�D�~�D���D�  D�>�D�~�D�� D�  D�AHD�� D���D�  D�@ D�~�D���D���D�>�D�~�D���D���D�>�D�� D��HD�HD�>�D�~�D��HD�  D�=qD�� D�� D�  D�AHD���D��HD�  D�@ D�� D���D�  D�AHD�� D���D���D�@ D��HD�� D�HD�AHD��HD�� D�  D�AHD��HD�� D��qD�=qD�~�D�� D�  D�@ D�� D�� D�  D�AHD�� D��qD���D�@ D�� D�� D�  D�>�D�� D�� D�  D�@ D�� D�� D���D�AHD�� D���D�  D�@ D�� D���D��qD�@ D��HD��HD�  D�>�D�� D��HD�HD�@ D�~�D�� D�HD�@ D�� D��HD�HD�AHD��HD�� D�  D�@ D��HD�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�>�D�~�D���D�HD�@ D�~�D�� D�HD�AHD�� D�� D�HD�AHD�� D�� D�  D�>�D�~�D�� D�HD�@ D�� D�� D�  D�@ D�� D���D�HD�@ D�� D�� D�  D�>�D�~�D�� D�  D�@ D��HD�� D�  D�@ D�� D���D���D�=qD�~�D��HD�HD�AHD�~�D�� D�  D�AHD��HD��HD�HD�AHD�� D�� D�  D�>�D�� D�� D�  D�@ D�}qD���D���D�@ D��HD���D�  D�@ D�~�D��qD�  D�@ D��HD�� D��qD�>�D�� D�� D�HD�@ D�� D�� D�  D�@ D�� D��HD�  D�@ D�� D���D�HD�AHD�� D��HD�HD�B�D��HD�� D��qD�>�D�~�D���D�  D�AHD�� D�� D�HD�>�D�� D�� D�  D�AHD�� D�� D�HD�AHD�� D�� D�  D�@ D�~�D�� D�  D�>�D�~�Dþ�D�  D�@ D�~�DĽqD���D�@ Dŀ Dž�D���D�@ Dƀ Dƾ�D���D�>�Dǀ D�� D���D�>�D�~�DȽqD�  D�@ Dɀ D�� D�  D�AHDʁHD��HD�HD�@ D�~�D˾�D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�HD�@ D�~�D�� D�HD�@ DρHD��HD���D�>�DЀ D�� D�  D�@ DсHD�� D�  D�>�DҀ D�� D�HD�AHDӀ DӾ�D���D�@ DԀ DԾ�D���D�@ D�~�D�� D�  D�>�D�~�D�� D�HD�AHD׀ D��HD�HD�AHD؁HD�� D��qD�>�Dـ D��HD�HD�>�D�~�D�� D�  D�AHDۀ D�� D�HD�AHD܁HD�� D�  D�@ D݀ Dݾ�D���D�>�D�~�D޾�D���D�=qD�~�D��HD�HD�@ D�� DྸD�  D�AHD� DᾸD�  D�@ D� D�� D�  D�@ D� D㾸D���D�>�D� D�� D�  D�AHD�HD��HD�  D�@ D�HD��HD�HD�AHD�~�D�� D�HD�@ D� D��HD���D�@ D� D�� D�  D�>�D� D꾸D�  D�@ D�HD��HD�  D�@ D�~�D�qD�HD�@ D� D��HD�  D�=qD� D��HD�HD�AHD� DﾸD�  D�@ D�� D�� D���D�>�D�HD��HD�  D�>�D�~�D�� D�  D�@ D�HD��HD���D�>�D�~�D���D���D�>�D�� D�� D�  D�@ D�� D�� D���D�>�D�~�D���D�  D�@ D�~�D�� D�HD�AHD�� D���D�  D�AHD�� D�� D�HD�>�D���?�?#�
?8Q�?aG�?�  ?�\)?��
?�{?�Q�?\?�
=?�ff?�@�@
=q@\)@z�@(�@#�
@+�@333@:�H@@  @G�@O\)@W
=@^�R@h��@n{@s33@z�H@�G�@��
@��@���@���@�33@�@���@�p�@�G�@�ff@��@�{@���@�33@�
=@��H@��R@\@�ff@���@���@�\)@�33@�
=@�(�@�  @��
@�ff@���@�@�\)@�z�@�Q�@�(�A   AG�A33Az�A
=A��A�Ap�A\)A��A�\A�
AAQ�A=qA(�A�RA ��A"�\A#�
A%A'�A)��A,(�A.�RA0  A1�A3�
A5�A7�A9��A;�A=p�A@  AAG�AC�
AE�AG�AI��AL(�AMp�AP  AQG�AS33AU�AW�AY��A\(�A]p�A_\)AaG�Ab�\Ae�Ag�Ai��Al(�Amp�Ao\)AqG�As33AuAw
=Ax��Az�HA|��A\)A���A���A��HA��
A�z�A�p�A�ffA�\)A���A���A��\A�33A�(�A��A�ffA�\)A�Q�A�G�A�=qA�33A�(�A��A�ffA�\)A�  A���A���A�33A�(�A��A�{A��RA��A���A��A��HA��
A���A�A��RA��A���A��A��HA��
A���A�p�A��RA��A���A��A��HA��
A���A�{A�
=A�Q�A�G�A�=qA�33A��
A��A�ffA�\)A�Q�A�G�A�=qA�33A�z�A�AƸRAǮAȣ�A�G�Aʏ\A��
A���A�AθRAϮAУ�A��A�33A�(�A��A�{A�
=A�  A���A�=qAۅA�(�A��A�{A�\)A�Q�AᙚA�\A�A�(�A�p�A�ffA�A��A陚A�\A�A�z�A�A�
=A�A��A�A��HA��
A���A�{A��RA��A���A��A�33A�(�A��A�{A�
=B (�B ��BG�BB{B�\B33B�Bz�B��Bp�BB=qB�HB�B�
BQ�B��B	p�B	�B
�\B
=B�B  Bz�B��Bp�B{B�\B33B�B  Bz�B��B��B=qB�RB33B�B(�B��BG�BB=qB�RB
=B�BQ�B��Bp�B�BffB�HB\)B  Bz�B��Bp�B�B�\B
=B�B (�B z�B!�B!��B"=qB"�RB#33B#�B$(�B$��B%�B%B&=qB&�HB'33B'�B((�B(��B)G�B)B*ffB*�HB+\)B+�B,(�B,��B-p�B-�B.ffB.�HB/\)B/�
B0Q�B0��B1��B2{B2�\B2�HB3\)B4  B4��B5G�B5��B6{B6�\B7
=B7�B8(�B8��B9�B9��B:{B:�RB;33B;�
B<(�B<��B=�B=B>=qB>�HB?\)B?�
B@Q�B@��BAp�BA�BB�\BC
=BC\)BC�
BDz�BD��BE��BF=qBF�\BG
=BG�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       ?�  @   @B�\@�  @��R@�  @�\A ��A  A#33A?\)A`��A�  A��A��A��A��A�Q�A�Q�A�Q�B (�B�
B�
B  B   B(  B0  B7�
B@  BH  BP  BX  B_�
Bh  Bp  Bw�B�
B��B�  B�{B�  B��B��B��B�  B�  B�  B�  B��B��B�  B��B�  B�{B�{B��B��B�  B�  B�  B��B��B��B��B��B�  B��B��C   C��C  C  C��C	��C��C  C  C��C  C��C��C  C
=C  C��C!�C$  C&  C'��C)��C,
=C.  C/��C1��C4  C6  C8
=C:
=C<
=C>  C?��CB  CD  CF  CH  CJ  CL  CM��CO��CQ��CT  CV
=CX  CY��C\  C^
=C`
=Cb  Cd  Ce��Cg��Cj  Cl  Cn
=Cp
=Cr
=Ct
=Cv
=Cw��Cy��C|  C~  C�  C���C���C�  C���C���C�  C�  C���C���C�  C�  C���C�  C���C�  C�C�  C���C���C�  C�C�  C���C�  C�  C�  C�C�C�C�C���C���C�  C�  C�  C�  C�  C���C���C�  C�  C�  C�  C�  C���C�  C�  C�C�  C�  C�C�
=C�  C�  C�  C�  C���C���C�  C�C�C�  C�  C�C�C�  C���C���C���C���C���C�  C�C�  C�  C�  C�  C�  C�  C�  C�  C�  C���C���C���C���C���C�  C�  C���C�  C�C�  C���C���C�C�  C���C���C�  C���C���C���C���C�  C���C���C�  C�  C���C�  C�  C�  C���C�  C�  C�  C�  C�  C���C���C�  C�C�C�C���C���D   D � D�D��D�qD}qD�qD}qD  D� D�qD��D�D� D�D� D�qD}qD�qD	}qD	�qD
z�D
��D}qD  D� D  D� D  D}qD  D��D�D� D  D� D�qD}qD�qD}qD  D��D  D��D�D��D�D� D  D� D  D� D  D� D�qD}qD�qD}qD  D}qD  D� D�D� D   D }qD �qD!� D"�D"� D"�qD#� D#�qD$}qD%  D%}qD%�qD&� D'�D'� D'�qD(� D)�D)��D*  D*� D+�D+� D,  D,� D-  D-��D.  D.}qD.�qD/}qD0  D0��D1  D1� D2  D2� D3�D3��D4  D4� D5  D5}qD5�qD6� D7  D7� D8�D8� D9  D9� D9�qD:}qD;  D;� D<  D<� D=  D=� D>�D>��D?�D?� D@  D@� D@�qDA}qDA�qDB� DC  DC}qDC�qDD� DE�DE� DE��DF� DG�DG� DH  DH� DI  DI��DJ�DJ� DK  DK��DL  DL� DM  DM}qDM�qDN}qDO  DO��DP  DP}qDP�qDQ}qDQ�qDR� DS  DS��DT�DT� DU  DU� DU�qDV��DW�DW��DXDX��DY�DY� DY�qDZ� D[�D[� D\  D\}qD]  D]� D]�qD^� D_  D_� D`  D`��Da�Da� Db  Db� Db�qDc� Dd  Dd� Dd�qDe� Df  Df� Dg  Dg� Dg�qDh}qDh�qDi� Di�qDj}qDk  Dk}qDk�qDl}qDm  Dm� Dn  Dn� Do  Do��Dp  Dp}qDq  Dq� Dr  Dr� Ds  Ds� Dt  Dt��Du�Du��Dv  Dv}qDv�qDw}qDx  Dx}qDx�qDy}qDy�qDz� D{  D{� D|�D|��D|�qD}}qD}�qD~}qD~�qD}qD�qD�@ D��HD�� D���D�@ D�� D�� D�  D�>�D�~�D��qD�  D�AHD�� D�� D�HD�@ D�� D��HD�  D�>�D�~�D���D�  D�>�D�~�D�� D�  D�AHD�� D���D�  D�@ D�~�D���D���D�>�D�~�D���D���D�>�D�� D��HD�HD�>�D�~�D��HD�  D�=qD�� D�� D�  D�AHD���D��HD�  D�@ D�� D���D�  D�AHD�� D���D���D�@ D��HD�� D�HD�AHD��HD�� D�  D�AHD��HD�� D��qD�=qD�~�D�� D�  D�@ D�� D�� D�  D�AHD�� D��qD���D�@ D�� D�� D�  D�>�D�� D�� D�  D�@ D�� D�� D���D�AHD�� D���D�  D�@ D�� D���D��qD�@ D��HD��HD�  D�>�D�� D��HD�HD�@ D�~�D�� D�HD�@ D�� D��HD�HD�AHD��HD�� D�  D�@ D��HD�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�>�D�~�D���D�HD�@ D�~�D�� D�HD�AHD�� D�� D�HD�AHD�� D�� D�  D�>�D�~�D�� D�HD�@ D�� D�� D�  D�@ D�� D���D�HD�@ D�� D�� D�  D�>�D�~�D�� D�  D�@ D��HD�� D�  D�@ D�� D���D���D�=qD�~�D��HD�HD�AHD�~�D�� D�  D�AHD��HD��HD�HD�AHD�� D�� D�  D�>�D�� D�� D�  D�@ D�}qD���D���D�@ D��HD���D�  D�@ D�~�D��qD�  D�@ D��HD�� D��qD�>�D�� D�� D�HD�@ D�� D�� D�  D�@ D�� D��HD�  D�@ D�� D���D�HD�AHD�� D��HD�HD�B�D��HD�� D��qD�>�D�~�D���D�  D�AHD�� D�� D�HD�>�D�� D�� D�  D�AHD�� D�� D�HD�AHD�� D�� D�  D�@ D�~�D�� D�  D�>�D�~�Dþ�D�  D�@ D�~�DĽqD���D�@ Dŀ Dž�D���D�@ Dƀ Dƾ�D���D�>�Dǀ D�� D���D�>�D�~�DȽqD�  D�@ Dɀ D�� D�  D�AHDʁHD��HD�HD�@ D�~�D˾�D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�HD�@ D�~�D�� D�HD�@ DρHD��HD���D�>�DЀ D�� D�  D�@ DсHD�� D�  D�>�DҀ D�� D�HD�AHDӀ DӾ�D���D�@ DԀ DԾ�D���D�@ D�~�D�� D�  D�>�D�~�D�� D�HD�AHD׀ D��HD�HD�AHD؁HD�� D��qD�>�Dـ D��HD�HD�>�D�~�D�� D�  D�AHDۀ D�� D�HD�AHD܁HD�� D�  D�@ D݀ Dݾ�D���D�>�D�~�D޾�D���D�=qD�~�D��HD�HD�@ D�� DྸD�  D�AHD� DᾸD�  D�@ D� D�� D�  D�@ D� D㾸D���D�>�D� D�� D�  D�AHD�HD��HD�  D�@ D�HD��HD�HD�AHD�~�D�� D�HD�@ D� D��HD���D�@ D� D�� D�  D�>�D� D꾸D�  D�@ D�HD��HD�  D�@ D�~�D�qD�HD�@ D� D��HD�  D�=qD� D��HD�HD�AHD� DﾸD�  D�@ D�� D�� D���D�>�D�HD��HD�  D�>�D�~�D�� D�  D�@ D�HD��HD���D�>�D�~�D���D���D�>�D�� D�� D�  D�@ D�� D�� D���D�>�D�~�D���D�  D�@ D�~�D�� D�HD�AHD�� D���D�  D�AHD�� D�� D�HD�>�D���?�?#�
?8Q�?aG�?�  ?�\)?��
?�{?�Q�?\?�
=?�ff?�@�@
=q@\)@z�@(�@#�
@+�@333@:�H@@  @G�@O\)@W
=@^�R@h��@n{@s33@z�H@�G�@��
@��@���@���@�33@�@���@�p�@�G�@�ff@��@�{@���@�33@�
=@��H@��R@\@�ff@���@���@�\)@�33@�
=@�(�@�  @��
@�ff@���@�@�\)@�z�@�Q�@�(�A   AG�A33Az�A
=A��A�Ap�A\)A��A�\A�
AAQ�A=qA(�A�RA ��A"�\A#�
A%A'�A)��A,(�A.�RA0  A1�A3�
A5�A7�A9��A;�A=p�A@  AAG�AC�
AE�AG�AI��AL(�AMp�AP  AQG�AS33AU�AW�AY��A\(�A]p�A_\)AaG�Ab�\Ae�Ag�Ai��Al(�Amp�Ao\)AqG�As33AuAw
=Ax��Az�HA|��A\)A���A���A��HA��
A�z�A�p�A�ffA�\)A���A���A��\A�33A�(�A��A�ffA�\)A�Q�A�G�A�=qA�33A�(�A��A�ffA�\)A�  A���A���A�33A�(�A��A�{A��RA��A���A��A��HA��
A���A�A��RA��A���A��A��HA��
A���A�p�A��RA��A���A��A��HA��
A���A�{A�
=A�Q�A�G�A�=qA�33A��
A��A�ffA�\)A�Q�A�G�A�=qA�33A�z�A�AƸRAǮAȣ�A�G�Aʏ\A��
A���A�AθRAϮAУ�A��A�33A�(�A��A�{A�
=A�  A���A�=qAۅA�(�A��A�{A�\)A�Q�AᙚA�\A�A�(�A�p�A�ffA�A��A陚A�\A�A�z�A�A�
=A�A��A�A��HA��
A���A�{A��RA��A���A��A�33A�(�A��A�{A�
=B (�B ��BG�BB{B�\B33B�Bz�B��Bp�BB=qB�HB�B�
BQ�B��B	p�B	�B
�\B
=B�B  Bz�B��Bp�B{B�\B33B�B  Bz�B��B��B=qB�RB33B�B(�B��BG�BB=qB�RB
=B�BQ�B��Bp�B�BffB�HB\)B  Bz�B��Bp�B�B�\B
=B�B (�B z�B!�B!��B"=qB"�RB#33B#�B$(�B$��B%�B%B&=qB&�HB'33B'�B((�B(��B)G�B)B*ffB*�HB+\)B+�B,(�B,��B-p�B-�B.ffB.�HB/\)B/�
B0Q�B0��B1��B2{B2�\B2�HB3\)B4  B4��B5G�B5��B6{B6�\B7
=B7�B8(�B8��B9�B9��B:{B:�RB;33B;�
B<(�B<��B=�B=B>=qB>�HB?\)B?�
B@Q�B@��BAp�BA�BB�\BC
=BC\)BC�
BDz�BD��BE��BF=qBF�\BG
=BG�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ĜA���A���A���A֡�A�VA�AՏ\A�/A��;Aԇ+A�r�A�ffA�bNA�XA�O�A�K�A�E�A�C�A�C�A�A�A�=qA�;dA�9XA�9XA�7LA�7LA�5?A�33A�33A�33A�/A�+A�(�A�5?A�~�A�%A֧�A׮A�K�A�7LA�+A��mA�r�A׃A׍PAְ!Aԉ7A���A�K�AԼjA�5?A���A���A�VA���A�|�A�I�A��A��#A�ffA��A� �Aȕ�A��A� �A�l�A��`AÝ�A�VA��RA�p�A�A��^A��A�XA���A�bA���A���A�?}A��
A��TA���A��A���A�oA��
A�ĜA��
A��jA��FA�JA��A��A�  A�r�A���A��A���A��A�9XA�VA�5?A��`A�r�A��FA�A�A�jA��A�l�A��\A��jA�JA���A���A���A��
A�`BA�oA��A��/A�n�A��\A��RA��HA��;A�ZA��-A��wA�G�A�;dA�A~I�A|ffAy��Ax��Av��Av �Au��AvbAvz�Av�\Av �Au��Au7LAt��At1'Ar��Aq�ApZAp�Ao��Am�-Aj�+Ag�AedZAc�Aa��A_�A\�AY��AY��AY�
AY�TAY��AY�AY�AW��AW7LAV�yAVAT�`AS�AS
=AQ|�ANjAK�AIt�AI?}AIC�AIXAI�AI�PAI��AI��AI��AI��AI��AI\)AF1AB~�A@��A@^5A@$�A=�A;�A:�A9S�A9K�A9/A9oA8��A8�A8��A8ĜA8ȴA8ĜA8^5A8JA6�A5G�A4��A4=qA3�A5�hA5�A4�A3��A2ĜA1x�A1oA0ȴA01'A/"�A.�A-��A,��A+;dA*n�A)ƨA)%A(��A'�wA'7LA'/A';dA&��A&r�A&�A$�/A$r�A#�FA"��A"$�A ��A ��A 9XA��AbNA-AG�AVA�A�PA%A�AZA$�AJA�FAA�;Av�A�A&�A"�A�AE�A�-AVAĜA  A�mA�PA"�A\)A��A�wA��A�A"�AVA�A��AĜA��A/A
ffA	�A=qA{A�hA�DA�
A33AVAA�AjAA�AJA�7A �@���@�n�@�&�@��@�r�@���@��#@�r�@�9X@��;@���@��j@�1'@�r�@��u@��
@�ff@�?}@���@@���@�5?@�x�@�Z@�@�\)@���@�R@�V@�?}@蛦@���@��@�@�^@�O�@�@��@�"�@�E�@��@���@��@��
@�\)@��@�ff@�`B@� �@��m@�33@�ff@�=q@�$�@��@��@�b@�33@�J@���@Չ7@Ցh@�@�5?@պ^@�Ĝ@�A�@��@��m@ӶF@��@�%@�Z@�  @ϕ�@�l�@�+@�
=@��y@�v�@�^5@�~�@Ͳ-@̴9@�bN@��@�J@�~�@��y@ʸR@�n�@��T@�p�@ȓu@ǅ@�\)@�@�=q@�@�7L@�(�@öF@�\)@�
=@�@\@��@�O�@�V@��/@��m@��@��@���@�`B@���@��@��@�r�@��/@��@��@�1@��@�@��`@�O�@�%@�z�@��m@�|�@�|�@�|�@��P@�\)@�
=@�ȴ@���@��+@��+@��\@�M�@��@�@�`B@�O�@�7L@�%@�%@�Ĝ@��u@�r�@�1'@���@�t�@��F@�ƨ@��F@�C�@�n�@�$�@�E�@��R@��@���@���@�bN@�ƨ@��F@��@�+@��@�+@�Z@�Ĝ@��9@��u@�Q�@��
@�b@���@�t�@�"�@�@���@��\@��\@��\@��\@���@���@��R@��R@�n�@�/@���@��D@��@�j@�Q�@�Z@�I�@�bN@��@��j@���@�bN@��D@��D@���@�A�@�A�@��@�  @��
@��@�S�@�
=@�@�"�@�C�@�;d@��@�dZ@�dZ@�\)@�|�@�|�@���@���@�l�@�|�@�K�@��!@��R@��H@��@��@��@�~�@�{@��#@���@���@���@���@��@���@���@�7L@�&�@�O�@��7@�x�@�p�@��h@��@�O�@�&�@�O�@�hs@���@���@��7@�x�@�O�@�/@��@�%@���@��`@��/@�Ĝ@��@��j@��j@���@��u@��D@�r�@�z�@��9@���@�Ĝ@��9@���@��@���@�j@�Q�@�9X@�I�@�Q�@�9X@�  @���@���@�K�@�t�@�
=@��@���@���@�ff@�E�@�{@�@���@���@�p�@�G�@�V@���@��@�Q�@�1'@�1@��m@��w@���@���@�l�@�S�@�33@�"�@�o@���@���@��+@�v�@�M�@�$�@�@�@��@�O�@��@���@�Ĝ@��9@���@�z�@�I�@�(�@��;@���@��P@�l�@�33@�
=@��H@�ȴ@��R@��!@��\@�M�@�$�@�J@��@�@���@���@��@�G�@�%@�Ĝ@���@���@��@�j@�Q�@�9X@�b@�1@��;@��w@���@�t�@��@�@���@���@�v�@�{@��-@�hs@�7L@�%@���@��/@��@�r�@�A�@�b@��w@�l�@�C�@�33@�o@�ȴ@�v�@�5?@�J@���@���@�@�@�@��^@��-@���@��7@�x�@�hs@�?}@��@���@��9@���@��@�Z@�1'@��@��@�b@�b@��@���@�t�@�dZ@�S�@�"�@�
=@�@�o@��@��@�o@�
=@�@���@��H@���@�M�@��^@�G�@�/@���@���@���@��@�&�@���@���@��@�I�@��@�ƨ@�|�@�\)@�S�@�;d@��@��@���@�v�@�5?@��T@���@���@�p�@�p�@�O�@�/@�V@��@��j@��u@�Z@�I�@�A�@�1@��m@���@�\)@�;d@�+@�@��@���@���@���@��\@�~�@�V@��@���@���@���@���@��h@��7@��@�`B@�V@��@��`@��/@��@��u@�bN@�A�@�b@��F@�dZ@�\)@�K�@�+@�
=@��H@�ȴ@���@��\@�v�@�ff@�5?@�$�@�@���@�@��-@��h@��7@��7@�p�@�X@�V@��D@�bN@�I�@�9X@��@��m@��@��@�\)@��@��@��y@���@��!@��+@�n�@�V@�5?@�@���@��-@�p�@�G�@�?}@�7L@�&�@��@�V@���@��/@���@��j@��@�A�@� �@�b@���@��
@��P@�K�@�
=@��H@���@�v�@�^5@�{@��#@��-@��7@�O�@�/@�&�@�V@���@���@��9@��u@�Q�@��@�@�w@��@l�@~��@~�R@~ff@~@}�T@}@}�-@}�@}p�@}`B@}`B@}?}@|�@|I�@|�@{�m@{�m@{ƨ@{t�@{33@{@z�!@z�\@zn�@zM�@z-@z�@y��@y��@y�7@y7L@x��@x��@x�u@xA�@x  @w�w@w��@wl�@wK�@w+@v��@v�y@vff@v$�@u@u`B@uV@t��@t�@t�j@tI�@s��@sƨ@s�F@s��@s��@st�@sS�@s@r�\@r-@rJ@q��@q��@qx�@qx�@qx�@qG�@p��@p�9@p�@pA�@p1'@pb@o�;@o��@o�@o�P@ol�@o+@nȴ@nff@n$�@n{@m�@m@mO�@mV@l��@l��@l�@l�D@lj@l�@k�
@k��@kt�@k"�@k@j��@j��@j^5@jM�@j-@i��@i�A���A�ȴA�ȴA���A־wA־wA�ȴA�ȴA�ĜA���A���A�ƨA�ƨA�ȴA���A���A���A�ȴA���A���A�A���A���A�AּjAֺ^Aִ9AֶFAּjAֺ^AָRA���A�ƨA���A�ȴA���Aִ9A֣�A֗�A֡�A֝�A֥�A֗�A�z�A�dZA�jA�x�A�t�A�~�A�ffA�C�A�$�A�$�A�&�A�&�A��A�{A�
=A���A��A��A���A��A��yAհ!AՕ�AՁA�z�A�x�A�t�A�jA�`BA�C�A�?}A�9XA�&�A�oA�VA�VA�bA�JA�
=A�
=A�A��`A��
A�ĜA�ƨAԲ-Aԩ�Aԝ�Aԏ\AԍPAԍPAԍPAԍPAԋDAԉ7Aԇ+AԅAԅAԇ+Aԉ7Aԉ7Aԇ+AԃA�|�A�z�A�z�A�|�A�|�A�z�A�v�A�t�A�r�A�r�A�r�A�r�A�r�A�p�A�n�A�l�A�l�A�l�A�n�A�l�A�hsA�hsA�jA�hsA�hsA�hsA�ffA�dZA�dZA�bNA�bNA�dZA�ffA�ffA�dZA�bNA�bNA�bNA�bNA�dZA�dZA�bNA�`BA�`BA�`BA�`BA�bNA�`BA�^5A�\)A�\)A�^5A�`BA�`BA�^5A�\)A�ZA�ZA�ZA�ZA�XA�S�A�Q�A�Q�A�Q�A�S�A�S�A�Q�A�Q�A�O�A�M�A�O�A�Q�A�Q�A�Q�A�M�A�M�A�M�A�O�A�Q�A�O�A�M�A�M�A�K�A�K�A�M�A�M�A�M�A�K�A�I�A�I�A�I�A�K�A�M�A�M�A�K�A�I�A�G�A�I�A�K�A�I�A�G�A�E�A�C�A�E�A�G�A�G�A�G�A�E�A�C�A�C�A�C�A�E�A�G�A�E�A�C�A�C�A�A�A�C�A�E�A�E�A�C�A�A�A�A�A�C�A�E�A�E�A�E�A�C�A�A�A�A�A�C�A�E�A�G�A�E�A�C�A�A�A�C�A�E�A�E�A�E�A�C�A�A�A�A�A�C�A�C�A�C�A�A�A�?}A�?}A�A�A�C�A�C�A�A�A�?}A�?}A�?}A�C�A�C�A�A�A�?}A�=qA�?}A�A�A�A�A�?}A�;dA�;dA�;dA�?}A�?}A�?}A�=qA�;dA�;dA�;dA�=qA�=qA�=qA�;dA�9XA�9XA�9XA�;dA�;dA�=qA�;dA�9XA�9XA�9XA�;dA�;dA�;dA�9XA�7LA�7LA�9XA�9XA�;dA�;dA�9XA�7LA�7LA�9XA�;dA�;dA�;dA�9XA�7LA�9XA�;dA�;dA�;dA�9XA�7LA�7LA�;dA�;dA�;dA�9XA�7LA�7LA�9XA�;dA�=qA�;dA�9XA�7LA�7LA�7LA�7LA�9XA�9XA�7LA�5?A�5?A�5?A�7LA�9XA�9XA�5?A�5?A�5?A�5?A�7LA�7LA�9XA�7LA�5?A�33A�5?A�7LA�9XA�9XA�7LA�5?A�5?A�5?A�5?A�5?A�5?A�33A�1'A�33A�33A�7LA�5?A�5?A�1'A�33A�5?A�5?A�7LA�33A�1'A�1'A�33A�33A�5?A�7LA�5?A�33A�1'A�33A�5?A�7LA�5?A�5?A�1'A�1'A�33A�33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       A�ĜA���A���A���A֡�A�VA�AՏ\A�/A��;Aԇ+A�r�A�ffA�bNA�XA�O�A�K�A�E�A�C�A�C�A�A�A�=qA�;dA�9XA�9XA�7LA�7LA�5?A�33A�33A�33A�/A�+A�(�A�5?A�~�A�%A֧�A׮A�K�A�7LA�+A��mA�r�A׃A׍PAְ!Aԉ7A���A�K�AԼjA�5?A���A���A�VA���A�|�A�I�A��A��#A�ffA��A� �Aȕ�A��A� �A�l�A��`AÝ�A�VA��RA�p�A�A��^A��A�XA���A�bA���A���A�?}A��
A��TA���A��A���A�oA��
A�ĜA��
A��jA��FA�JA��A��A�  A�r�A���A��A���A��A�9XA�VA�5?A��`A�r�A��FA�A�A�jA��A�l�A��\A��jA�JA���A���A���A��
A�`BA�oA��A��/A�n�A��\A��RA��HA��;A�ZA��-A��wA�G�A�;dA�A~I�A|ffAy��Ax��Av��Av �Au��AvbAvz�Av�\Av �Au��Au7LAt��At1'Ar��Aq�ApZAp�Ao��Am�-Aj�+Ag�AedZAc�Aa��A_�A\�AY��AY��AY�
AY�TAY��AY�AY�AW��AW7LAV�yAVAT�`AS�AS
=AQ|�ANjAK�AIt�AI?}AIC�AIXAI�AI�PAI��AI��AI��AI��AI��AI\)AF1AB~�A@��A@^5A@$�A=�A;�A:�A9S�A9K�A9/A9oA8��A8�A8��A8ĜA8ȴA8ĜA8^5A8JA6�A5G�A4��A4=qA3�A5�hA5�A4�A3��A2ĜA1x�A1oA0ȴA01'A/"�A.�A-��A,��A+;dA*n�A)ƨA)%A(��A'�wA'7LA'/A';dA&��A&r�A&�A$�/A$r�A#�FA"��A"$�A ��A ��A 9XA��AbNA-AG�AVA�A�PA%A�AZA$�AJA�FAA�;Av�A�A&�A"�A�AE�A�-AVAĜA  A�mA�PA"�A\)A��A�wA��A�A"�AVA�A��AĜA��A/A
ffA	�A=qA{A�hA�DA�
A33AVAA�AjAA�AJA�7A �@���@�n�@�&�@��@�r�@���@��#@�r�@�9X@��;@���@��j@�1'@�r�@��u@��
@�ff@�?}@���@@���@�5?@�x�@�Z@�@�\)@���@�R@�V@�?}@蛦@���@��@�@�^@�O�@�@��@�"�@�E�@��@���@��@��
@�\)@��@�ff@�`B@� �@��m@�33@�ff@�=q@�$�@��@��@�b@�33@�J@���@Չ7@Ցh@�@�5?@պ^@�Ĝ@�A�@��@��m@ӶF@��@�%@�Z@�  @ϕ�@�l�@�+@�
=@��y@�v�@�^5@�~�@Ͳ-@̴9@�bN@��@�J@�~�@��y@ʸR@�n�@��T@�p�@ȓu@ǅ@�\)@�@�=q@�@�7L@�(�@öF@�\)@�
=@�@\@��@�O�@�V@��/@��m@��@��@���@�`B@���@��@��@�r�@��/@��@��@�1@��@�@��`@�O�@�%@�z�@��m@�|�@�|�@�|�@��P@�\)@�
=@�ȴ@���@��+@��+@��\@�M�@��@�@�`B@�O�@�7L@�%@�%@�Ĝ@��u@�r�@�1'@���@�t�@��F@�ƨ@��F@�C�@�n�@�$�@�E�@��R@��@���@���@�bN@�ƨ@��F@��@�+@��@�+@�Z@�Ĝ@��9@��u@�Q�@��
@�b@���@�t�@�"�@�@���@��\@��\@��\@��\@���@���@��R@��R@�n�@�/@���@��D@��@�j@�Q�@�Z@�I�@�bN@��@��j@���@�bN@��D@��D@���@�A�@�A�@��@�  @��
@��@�S�@�
=@�@�"�@�C�@�;d@��@�dZ@�dZ@�\)@�|�@�|�@���@���@�l�@�|�@�K�@��!@��R@��H@��@��@��@�~�@�{@��#@���@���@���@���@��@���@���@�7L@�&�@�O�@��7@�x�@�p�@��h@��@�O�@�&�@�O�@�hs@���@���@��7@�x�@�O�@�/@��@�%@���@��`@��/@�Ĝ@��@��j@��j@���@��u@��D@�r�@�z�@��9@���@�Ĝ@��9@���@��@���@�j@�Q�@�9X@�I�@�Q�@�9X@�  @���@���@�K�@�t�@�
=@��@���@���@�ff@�E�@�{@�@���@���@�p�@�G�@�V@���@��@�Q�@�1'@�1@��m@��w@���@���@�l�@�S�@�33@�"�@�o@���@���@��+@�v�@�M�@�$�@�@�@��@�O�@��@���@�Ĝ@��9@���@�z�@�I�@�(�@��;@���@��P@�l�@�33@�
=@��H@�ȴ@��R@��!@��\@�M�@�$�@�J@��@�@���@���@��@�G�@�%@�Ĝ@���@���@��@�j@�Q�@�9X@�b@�1@��;@��w@���@�t�@��@�@���@���@�v�@�{@��-@�hs@�7L@�%@���@��/@��@�r�@�A�@�b@��w@�l�@�C�@�33@�o@�ȴ@�v�@�5?@�J@���@���@�@�@�@��^@��-@���@��7@�x�@�hs@�?}@��@���@��9@���@��@�Z@�1'@��@��@�b@�b@��@���@�t�@�dZ@�S�@�"�@�
=@�@�o@��@��@�o@�
=@�@���@��H@���@�M�@��^@�G�@�/@���@���@���@��@�&�@���@���@��@�I�@��@�ƨ@�|�@�\)@�S�@�;d@��@��@���@�v�@�5?@��T@���@���@�p�@�p�@�O�@�/@�V@��@��j@��u@�Z@�I�@�A�@�1@��m@���@�\)@�;d@�+@�@��@���@���@���@��\@�~�@�V@��@���@���@���@���@��h@��7@��@�`B@�V@��@��`@��/@��@��u@�bN@�A�@�b@��F@�dZ@�\)@�K�@�+@�
=@��H@�ȴ@���@��\@�v�@�ff@�5?@�$�@�@���@�@��-@��h@��7@��7@�p�@�X@�V@��D@�bN@�I�@�9X@��@��m@��@��@�\)@��@��@��y@���@��!@��+@�n�@�V@�5?@�@���@��-@�p�@�G�@�?}@�7L@�&�@��@�V@���@��/@���@��j@��@�A�@� �@�b@���@��
@��P@�K�@�
=@��H@���@�v�@�^5@�{@��#@��-@��7@�O�@�/@�&�@�V@���@���@��9@��u@�Q�@��@�@�w@��@l�@~��@~�R@~ff@~@}�T@}@}�-@}�@}p�@}`B@}`B@}?}@|�@|I�@|�@{�m@{�m@{ƨ@{t�@{33@{@z�!@z�\@zn�@zM�@z-@z�@y��@y��@y�7@y7L@x��@x��@x�u@xA�@x  @w�w@w��@wl�@wK�@w+@v��@v�y@vff@v$�@u@u`B@uV@t��@t�@t�j@tI�@s��@sƨ@s�F@s��@s��@st�@sS�@s@r�\@r-@rJ@q��@q��@qx�@qx�@qx�@qG�@p��@p�9@p�@pA�@p1'@pb@o�;@o��@o�@o�P@ol�@o+@nȴ@nff@n$�@n{@m�@m@mO�@mV@l��@l��@l�@l�D@lj@l�@k�
@k��@kt�@k"�@k@j��@j��@j^5@jM�@j-@i��@i�A���A�ȴA�ȴA���A־wA־wA�ȴA�ȴA�ĜA���A���A�ƨA�ƨA�ȴA���A���A���A�ȴA���A���A�A���A���A�AּjAֺ^Aִ9AֶFAּjAֺ^AָRA���A�ƨA���A�ȴA���Aִ9A֣�A֗�A֡�A֝�A֥�A֗�A�z�A�dZA�jA�x�A�t�A�~�A�ffA�C�A�$�A�$�A�&�A�&�A��A�{A�
=A���A��A��A���A��A��yAհ!AՕ�AՁA�z�A�x�A�t�A�jA�`BA�C�A�?}A�9XA�&�A�oA�VA�VA�bA�JA�
=A�
=A�A��`A��
A�ĜA�ƨAԲ-Aԩ�Aԝ�Aԏ\AԍPAԍPAԍPAԍPAԋDAԉ7Aԇ+AԅAԅAԇ+Aԉ7Aԉ7Aԇ+AԃA�|�A�z�A�z�A�|�A�|�A�z�A�v�A�t�A�r�A�r�A�r�A�r�A�r�A�p�A�n�A�l�A�l�A�l�A�n�A�l�A�hsA�hsA�jA�hsA�hsA�hsA�ffA�dZA�dZA�bNA�bNA�dZA�ffA�ffA�dZA�bNA�bNA�bNA�bNA�dZA�dZA�bNA�`BA�`BA�`BA�`BA�bNA�`BA�^5A�\)A�\)A�^5A�`BA�`BA�^5A�\)A�ZA�ZA�ZA�ZA�XA�S�A�Q�A�Q�A�Q�A�S�A�S�A�Q�A�Q�A�O�A�M�A�O�A�Q�A�Q�A�Q�A�M�A�M�A�M�A�O�A�Q�A�O�A�M�A�M�A�K�A�K�A�M�A�M�A�M�A�K�A�I�A�I�A�I�A�K�A�M�A�M�A�K�A�I�A�G�A�I�A�K�A�I�A�G�A�E�A�C�A�E�A�G�A�G�A�G�A�E�A�C�A�C�A�C�A�E�A�G�A�E�A�C�A�C�A�A�A�C�A�E�A�E�A�C�A�A�A�A�A�C�A�E�A�E�A�E�A�C�A�A�A�A�A�C�A�E�A�G�A�E�A�C�A�A�A�C�A�E�A�E�A�E�A�C�A�A�A�A�A�C�A�C�A�C�A�A�A�?}A�?}A�A�A�C�A�C�A�A�A�?}A�?}A�?}A�C�A�C�A�A�A�?}A�=qA�?}A�A�A�A�A�?}A�;dA�;dA�;dA�?}A�?}A�?}A�=qA�;dA�;dA�;dA�=qA�=qA�=qA�;dA�9XA�9XA�9XA�;dA�;dA�=qA�;dA�9XA�9XA�9XA�;dA�;dA�;dA�9XA�7LA�7LA�9XA�9XA�;dA�;dA�9XA�7LA�7LA�9XA�;dA�;dA�;dA�9XA�7LA�9XA�;dA�;dA�;dA�9XA�7LA�7LA�;dA�;dA�;dA�9XA�7LA�7LA�9XA�;dA�=qA�;dA�9XA�7LA�7LA�7LA�7LA�9XA�9XA�7LA�5?A�5?A�5?A�7LA�9XA�9XA�5?A�5?A�5?A�5?A�7LA�7LA�9XA�7LA�5?A�33A�5?A�7LA�9XA�9XA�7LA�5?A�5?A�5?A�5?A�5?A�5?A�33A�1'A�33A�33A�7LA�5?A�5?A�1'A�33A�5?A�5?A�7LA�33A�1'A�1'A�33A�33A�5?A�7LA�5?A�33A�1'A�33A�5?A�7LA�5?A�5?A�1'A�1'A�33A�33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BǮBǮBǮBŢBƨBÖB�}B��B�jB�^B�LB�LB�LB�LB�LB�LB�LB�LB�LB�LB�LB�LB�LB�LB�RB�RB�RB�RB�RB�RB�XB�RB�^B�wB��B'�B�B;dB�NBE�BT�Bn�Bx�B�+B��B��B�}B�VB�JBĜB8RBXBz�Bx�B�B�BbB�B�B�B1'B=qBA�BD�BF�B=qB<jB;dB-B%�B�B�B�
B�qB��B�hB�JB�Bx�BW
B1B��B�LB��B�Be`B+BVB��B�B�qB�{BdZB33B=qB(�B�B�BB��BB
=B%�B7LB=qB33B�B��B��B��B�}B��B�ZB�#B�XB��B{�BjBhsBn�B`BB9XB'�B�BB�B�BǮB�XB��B��B��B�{B~�Bm�BJ�B;dB)�B!�B�B(�B2-B;dB9XB5?B/B'�B�BoBB��B�B�B�B�B�=Bq�BO�BF�B&�B�B�HB�HB�HB�NB�BB�/B�B��B��B��BB�XB��B��B�7Bl�BQ�B8RB2-B2-B33B6FB7LB8RB9XB8RB7LB6FB33B�B�`B��B��BƨB�LB��B�DB�B�B�B� B}�Bz�B�B�B�B�B� Bz�Bq�BaHB[#B^5B[#By�B~�B{�Bp�BhsB[#BT�BP�BL�B>wB;dB/B%�B\B
=B��B��B�B�sB�fB�yB�B�sB�`B�NB�B��B��B��B�dB�B��B��B��B�uB�\B�=B�B�Bu�Bn�BcTBW
BS�BR�BN�BH�B>wB2-B(�B$�B#�B!�B�B�BhB\B	7B1BBBB	7BJBVBPB
=BB��B��B��B�B�yB�ZB�/B��B��BȴB��B�XB�9B�B��B��B��B��B��B�{B�JB�B~�Bz�Bx�Bx�Bt�Bo�BjBhsBgmB`BB\)BZB]/B]/B^5BYBS�BQ�BM�BI�BG�BD�BA�B?}B=qB;dB;dB9XB6FB33B0!B.B(�B&�B%�B#�B �B�B�B�B{B{BoBbBVBPBDB+B%BBBBBB
��B
��B
��B
��B
�B
�B
�B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�sB
�mB
�fB
�fB
�`B
�sB
�B
�yB
�yB
�B
�yB
�`B
�TB
�BB
�/B
�HB
�`B
�fB
�sB
�mB
�fB
�ZB
�NB
�NB
�NB
�HB
�NB
�BB
�/B
�#B
�B
�B
�B
�B
�B
��B
��B
�
B
��B
��B
ɺB
ǮB
��B
��B
��B
��B
��B
��B
�B
�B
��B
��B
ɺB
ȴB
��B
��B
��B
��B
ɺB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�
B
�
B
�
B
�
B
�B
�B
�)B
�5B
�BB
�BB
�BB
�;B
�BB
�;B
�5B
�NB
�ZB
�`B
�ZB
�HB
�HB
�NB
�ZB
�B
��B
��B
��B
�B
�B
�B
�B
�B
�B
��BBBBBBBBBB  B  BBBB%B1B1B	7B
=B
=B	7B1B1B1B1B	7BDBJBVBbBuB�B�B�B�B�B �B!�B!�B!�B"�B"�B"�B#�B$�B'�B+B,B.B2-B5?B8RB;dB=qB?}BA�BC�BE�BF�BF�BF�BJ�BN�BP�BQ�BP�BP�BQ�BR�BR�BS�BVBW
B[#B^5B^5B^5B`BBcTBe`BhsBk�Bn�Bn�Bo�Br�Bt�Bw�B{�B}�B~�B�B�B�B�+B�1B�7B�=B�DB�JB�VB�bB�hB�hB�uB�uB��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�-B�-B�3B�3B�?B�FB�?B�?B�?B�FB�FB�LB�LB�RB�XB�dB�jB�wB�}B��BBÖBĜBĜBŢBŢBƨBƨBǮBǮBȴBȴBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�
B�B�B�B�B�#B�#B�#B�#B�)B�/B�5B�5B�5B�;B�BB�BB�BB�HB�HB�NB�TB�TB�TB�TB�ZB�`B�fB�fB�mB�sB�sB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  BBBBBBBBBB  B��B��B��B  B  BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB  B  B  B  B  B  B  B  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�yB�yB�yB�yB�yB�yB�sB�sB�sB�sB�sB�sB�sB�sB�mB�mB�mB�mB�mB�mB�mB�mB�fB�fB�fB�fB�fB�fB�fB�`B�`B�`B�`B�`B�`B�`B�`B�ZB�ZB�ZB�ZB�ZB�ZB�ZB�ZB�ZB�TB�ZB�TB�TB�TB�TB�TB�TB�NB�NB�NB�NB�NB�HBǮBȴBȴBɺBŢB��BŢBƨBȴBƨBǮBȴBȴBȴBȴBƨBȴBǮBŢB��BɺBȴBƨBƨBǮBŢBƨBƨBƨBŢBŢBÖBŢBBƨBǮBɺBȴBÖBBĜB��B��B��BĜB�qB��BB�qB��B��BŢB��B�wB�}B��B�}BB��B��B�^B��B�qBBɺBÖB��B�wB�jB�XB�wB�dB��B�XB�dB��B�qB�dB�RB�LB�RB�LB�LB�dB�}B�jB�dB�?B�qB�LB�dB�XB�RB�LB�FB�FB�FB�FB�RB�RB�RB�LB�LB�FB�FB�RB�RB�RB�RB�LB�FB�LB�LB�RB�RB�RB�LB�LB�LB�LB�RB�RB�RB�LB�LB�FB�RB�RB�LB�LB�FB�FB�FB�LB�LB�RB�LB�FB�FB�FB�LB�LB�LB�LB�LB�FB�FB�LB�LB�RB�LB�LB�FB�FB�LB�RB�RB�LB�FB�FB�LB�RB�RB�RB�LB�FB�LB�LB�LB�RB�LB�FB�FB�FB�LB�LB�RB�LB�FB�FB�FB�LB�LB�LB�LB�?B�FB�FB�FB�LB�LB�FB�FB�FB�FB�LB�LB�LB�FB�FB�FB�LB�LB�LB�FB�FB�FB�LB�LB�RB�LB�FB�FB�FB�LB�LB�RB�LB�FB�FB�FB�LB�LB�RB�LB�FB�FB�LB�RB�RB�RB�LB�FB�LB�LB�RB�RB�LB�LB�FB�LB�LB�RB�RB�LB�LB�LB�RB�RB�RB�LB�LB�LB�RB�RB�RB�LB�LB�FB�LB�RB�RB�RB�LB�FB�LB�RB�RB�RB�LB�FB�LB�RB�XB�RB�LB�FB�LB�LB�RB�RB�RB�LB�LB�LB�LB�RB�RB�RB�LB�LB�FB�LB�RB�RB�RB�LB�LB�LB�RB�RB�RB�LB�LB�LB�FB�LB�RB�RB�RB�LB�LB�LB�RB�RB�RB�LB�LB�LB�RB�RB�RB�LB�LB�LB�RB�RB�RB�LB�LB�LB�LB�RB�RB�RB�RB�LB�LB�LB�RB�RB�XB�RB�RB�LB�LB�RB�XB�XB�RB�RB�LB�LB�LB�RB�XB�RB�RB�LB�LB�LB�RB�XB�XB�RB�LB�LB�RB�RB�RB�RB�LB�LB�RB�XB�RB�RB�LB�LB�RB�XB�XB�RB�RB�LB�LB�LB�RB�XB�RB�RB�LB�LB�RB�XB�XB�XB�RG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       BǚB�B�B�8B�cBƧBóB��B��B�B��B��B�aB�zB�rB�_B�gB�VB�LB�VB�^B�XB�XB�SB�YB�QB�[B�[B�NB�VB�cB�mB�eB�AB�XB"�B~�B6�BޮBE`BUfBo�Bz�B�B��BăBȴB��B��B�>B:�B^�B��B|�B��B�B�B�BB#B8=BAWBDCBGBK\BAVBC�BAdB/
B-�B!�B��B�EB��B��B�9B�CB��B�Bg�B\B��B�VB��B�rBrSB2�B}B�(B��B�fB��BsB7BD�B,B!�B�B�B��BsBnB%&B8�B@7B<FB�B RB��B��B��B�:B�-B�B�4B�)B�/Bm+BjEBq�BhRB<GB,�B*B�B��B�'BˊB�`B�^B�0B� B��B�DBs�BN�B?�B,oB##B�B'�B2 B<oB:�B6~B0�B)OB#�B�BDB��B�?B�BްB��B�VBw�BS�BLB/}BnB�)B�<B�.B�B�BބB�B��B��B�@BŸB�B��B�OB��BtsBW�B9NB25B1�B2�B6B7!B8IB9qB8QB7RB7qB</B"B� B��B��B�2B��B��B�qB�TB�`B�eB�6B~@BzVB��B��B�#B�.B�B~	Bv2BcZB\RB^�BV�By�B��B,Bs�Bk�B\TBU�BR�BO�B?lB>�B2"B*B�B B B�B�B�	B�B�rB�)B�$B�yB�B�pB�&B��B�3B�>B��B�BB�RB�pB�@B��B��B�aB�Bw�Bs�Be�BW�BTUBS�BP�BK�BB�B5B*B%B$�B#uBbBpB`B�B	�B	.BEBaB{B�B�B�B�B�B�B��B�-B�jB�B��B�B�BТB�^BˬB¦B�TB��B��B��B��B�9B�HB�.B��B��B��B��B{�ByQB{8Bv#Bq�Bj�BiBjUBa�B\�BY�B]B^VB`NBZ�BT�BS�BO#BJ�BH�BF;BB�B@B>:B;�B< B:�B7DB4jB1;B/�B)oB'�B'B%B!�BBcBGB�B|B/BB6B�B"B�B6BEBXB5BoB:B �B
�>B
�zB
�B
�"B
�B
�gB
�"B
��B
�.B
��B
�,B
�B
�B
� B
�CB
�B
�B
�B
�B
��B
�B
�B
�B
�B
�]B
�B
��B
��B
�oB
�B
܌B
�B
�B
��B
�<B
�&B
�B
��B
�B
��B
�`B
�B
�B
��B
��B
۲B
ڜB
ٶB
�>B
��B
��B
�jB
�WB
�zB
ՊB
ҏB
�yB
ưB
̂B
ҭB
ІB
�&B
�IB
ӘB
�B
פB
��B
�HB
��B
�:B
�2B
ϪB
ͩB
�_B
��B
��B
̽B
�#B
�`B
�CB
�B
�B
��B
��B
�gB
מB
�QB
לB
�0B
�@B
�nB
�<B
��B
��B
�vB
߬B
�B
�uB
��B
�.B
�xB
�B
�B
�B
�B
�B
��B
�"B
�B
�'B
��B
��B
��B
�,B
��B
�9B
�B
�@B BDBxB�B �B;B�B�B@B bB LB
BBBB!BB	?B
�BB
BTB?BTBXB	/BYB'B"BB]B'BYB�B�B JB �B"B!�B"B#NB#!B#>B#�B$�B'�B+B,4B-�B2&B5HB8$B;^B=8B?�BA�BC�BE�BG�BF�BFtBJkBN�BQ)BR�BQ�BQBBRDBR�BR�BS�BV*BV�B[cB^�B^NB]�B_�BcjBelBhGBk�Bn�Bn�BojBr�BtiBw�B|B~B>B�<B�2B�HB�CB�LB�EB�bB�fB�4B�YB��B�B�|B��B�hB�.B�qB��B��B��B��B��B�:B�B�B��B� B�@B�B��B�FB��B�B��B��B�iB�sB��B�zB��B�iB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B��B�
B�B�B�BB�FB�0B�4B�+B�6B�B�B�6B�>B�1B�mB�]B�4B�FB�kB�VB�^B�HB�=B�5B�]B܍B�lB�[B�`BށB�lB�PB�jB�B�B�B�B�_B�zB�zB�}B�B�B�sB�B�B�B��B� B�B��B�B��B�B�"B�B��B��B�B��B��B��B��B��B� B�$B��B��B��B�B�/B�B��B��B��B��B�B��B��B��B��B��B��B��B�B��B��B�8B��B��B�B�B��B��B��B��B�B�TB�.B�B�B�4B�B��B��B��BBBBBB2BtB�B�B �B�'B�FB��B B��B�BSB\B`B�B�B^B�BRB,BEBUBbB�BoB�B�BCBdBXBBJBNBQBPBdBXBpB4B'BnBQB|B�BIB2BRBOB6B6BB*B,BRBoB�B@BBBBBBAB�B=BBBUB3BRB=BUB�B�BB B 3B 3B ;B &B B KB $B�B�EB�B�2B�DB�B�B�(B�B��B�B�B�lB��B�5B�B�B�B�6B�>B�(B�*B�KB��B�.B�B��B�"B�B�B�B�,B�9B�	B�>B�B��B��B��B��B��B��B�B��B��B�.B�6B�B��B��B�B�;B�0B�.B�B�B�B��B�-B�B��B��B�B��B��B��B��B��B��B��B�B�B��B��B��B��B��B��B��B��B�B�B�B��B�B�B�B��B�B��B��B�B�B�B��B��B�B��B��B��B��B��B��B��B��B��B��B�B�B�B��B��B�B�B�B�B�B�B�B��B�B��B��B�B�B�B�B��B�B�B�B�B�~B�B�B�B��B�B�B�B�B�B�mB�pB�B�B�B�B�B�vB�B�B�tB�}B�zB�|B�B�B�B�B�lB�tB�B�B�B�lB�B�wB�vB�tB�B�B�}B�{B�B�rB�{B�{B�B�ZB�gB�tB�XB�HBǮBȴBȴBɺBŢB��BŢBƨBȴBƨBǮBȴBȴBȴBȴBƨBȴBǮBŢB��BɺBȴBƨBƨBǮBŢBƨBƨBƨBŢBŢBÖBŢBBƨBǮBɺBȴBÖBBĜB��B��B��BĜB�qB��BB�qB��B��BŢB��B�wB�}B��B�}BB��B��B�^B��B�qBBɺBÖB��B�wB�jB�XB�wB�dB��B�XB�dB��B�qB�dB�RB�LB�RB�LB�LB�dB�}B�jB�dB�?B�qB�LB�dB�XB�RB�LB�FB�FB�FB�FB�RB�RB�RB�LB�LB�FB�FB�RB�RB�RB�RB�LB�FB�LB�LB�RB�RB�RB�LB�LB�LB�LB�RB�RB�RB�LB�LB�FB�RB�RB�LB�LB�FB�FB�FB�LB�LB�RB�LB�FB�FB�FB�LB�LB�LB�LB�LB�FB�FB�LB�LB�RB�LB�LB�FB�FB�LB�RB�RB�LB�FB�FB�LB�RB�RB�RB�LB�FB�LB�LB�LB�RB�LB�FB�FB�FB�LB�LB�RB�LB�FB�FB�FB�LB�LB�LB�LB�?B�FB�FB�FB�LB�LB�FB�FB�FB�FB�LB�LB�LB�FB�FB�FB�LB�LB�LB�FB�FB�FB�LB�LB�RB�LB�FB�FB�FB�LB�LB�RB�LB�FB�FB�FB�LB�LB�RB�LB�FB�FB�LB�RB�RB�RB�LB�FB�LB�LB�RB�RB�LB�LB�FB�LB�LB�RB�RB�LB�LB�LB�RB�RB�RB�LB�LB�LB�RB�RB�RB�LB�LB�FB�LB�RB�RB�RB�LB�FB�LB�RB�RB�RB�LB�FB�LB�RB�XB�RB�LB�FB�LB�LB�RB�RB�RB�LB�LB�LB�LB�RB�RB�RB�LB�LB�FB�LB�RB�RB�RB�LB�LB�LB�RB�RB�RB�LB�LB�LB�FB�LB�RB�RB�RB�LB�LB�LB�RB�RB�RB�LB�LB�LB�RB�RB�RB�LB�LB�LB�RB�RB�RB�LB�LB�LB�LB�RB�RB�RB�RB�LB�LB�LB�RB�RB�XB�RB�RB�LB�LB�RB�XB�XB�RB�RB�LB�LB�LB�RB�XB�RB�RB�LB�LB�LB�RB�XB�XB�RB�LB�LB�RB�RB�RB�RB�LB�LB�RB�XB�RB�RB�LB�LB�RB�XB�XB�RB�RB�LB�LB�LB�RB�XB�RB�RB�LB�LB�RB�XB�XB�XB�RG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       <#�C<#�<#��<%�<)��<+><1&�<+�!<(,�<&4p<$ <#��<#�c<#�<#�r<#�$<#�D<#�X<#�
<#�X<#�<#�{<#�{<#�0<#�0<#�<#�I<#�I<#�<#�<#�i<#�D<#�0<#��<%�L<9�<7E<<5SL<-͝<#�U<#�	<$�<'><#�$<#ا</�{<\ik<,��<%G<%K:<(�)<C�U<Y�_</�<%�~<@��<:'�<$�<%��<I��<G=�</O�<)�i<(�(<4Kd</J<Jӭ<=�<&�U<OB�<p�<ZJ�<g��<S�u<V6�<.�X<*��<)Ɩ<g'�<�-�<~t0<6��<6�l<bP�<J!<��<Ka�<�M�<9>8<%�d<d��<]59<���<.�X<Hș<+'�<)��<1�"<*F�<*��<21<$��<$C�<%�b<)�5<Z��<Q:0<_�W</��<%>�<$�<$$<.�<E��<5Z'<A\G<1�M<)SQ<&h�<*�<P��<*nL<3�Z<3]�<4<8�Z<+�</<6�M<(��<$.<(��<-�<8g~<?e4<1 �<2�E<(v<%>�<$�q<$ح<#�8<$�	<% <%s<%��<%MY<13+<1�<'��<$_�<%�<6�<V��<U+�<>�-<?#)<.>�<9\<U�<<N�><$q@<#�{<#�<#�&<$N�<%8j<0Z<$�!<$�V<(v<+v�<.DP<%�<3�<U�[<N�<=�<$�e<#�<<#�<#�N<#ۮ<#ܯ<#�I<#��<#�<#�&<$��<Y�_<_Q�<4��<&��<$ѩ<Ktz<A�&<*Ѝ<'d<#�U<#�<#�5<$�L<#�<$�<#�*<#ף<#ا<$��<$�3<+r9<3*<''�<$�J<$}<1�k<#��<%�`<+�!<+�<-4�<$��<$�b<&)�<)��<$��<+�c<*ٜ<1&�<(\,<&�
<'k�<%K:<(�<%�<#��<#�0<%�<$6�<$�R<,$;<%Oz<'�<*�<&�</�<$	�<% <,��<&A�<$T�<(��<$=<%<*�<&h�<8�Z<'��<$L<#�m<$�e<'*�<, <01�<*nL<$��<#�8<$v�<%�@<&y<&�
<$�Q<'��<#�)<$��<$��<$0.<$)
<$ �<#�<#�<$��<(\,<-*�<$Y�<#��<)?0<&�8<()+<)�6<-%b<$J�<%��<*�-<'^m<&�U<(�<&4p<&��<&�/<$�<$3U<%�j<(��<)�<&e�<&�^<$><<$�<(;B<%^�<'F<#��<$0.<*O�<%�<$Y�<#�<#ܯ<$��<'><&�<$2G<&]p<%.+<$z�<$ح<%�`<$�X<$�<$R'<#ۮ<$!><%�<$�w<$�-<$�<%��<$�<$2G<%�<%S<$Y�<%8j<&/<$@|<#�<$�<$Gd<$+<$o�<%��<&��<$/<$��<$��<#�<#��<#��<$��<%�<%<�<%�~<#�5<#��<#�I<#�<$Z<$@|<%Z2<$Sa<$�<#�<#��<%:{<)7,<$��<$}<$Z<#�<#��<#�&<#�N<$%<#��<#�o<$��<%y<$k<'Dv<%2?<$(<$�<#��<$<<$R'<$?[<%&<%��<#�<$�<$��<#��<$�k<%��<$A�<$f<$<<$$<#�<$��<$z�<#��<#�<%rN<%�Z<)c�<$F9<$�w<#�4<$H�<$,<#�!<$F<#�(<#�{<%��<*e<$	�<%�<$p<#�<$\"<$j|<$}<#�<<#�<#��<#��<$/<#��<#��<#��<#�<#�<#�m<$�<#�l<$
<#��<#�+<#�<#�0<#�N<#�<#�J<#��<$^�<#�N<#�<#�*<#��<$7�<$��<#�N<#�J<$=<%e<%U�<#ޫ<#��<$q@<#�r<#�<$
�<#�<#��<&�<$9�<#ף<#ޫ<#��<$.<#�<#�+<$I�<$p<#�N<#�W<#�<#�<#�<#�<#��<#��<#ا<#�<<$�<&e�<$c�<#��<#ף<#��<#ۮ<#�<<#�c<#��<#�J<#�<#��<$Z<#�<#�<#��<$�<#�<<#��<#�+<#�<$�<#�<#�H<#�i<#ܯ<#��<#�X<#��<#��<#�0<#�I<#�<#�&<#��<#��<#�N<#�<#�<$o�<#�X<#�J<#�<#�&<#�&<$1:<$)
<#�m<#�<#�{<#�<#�0<#�r<#��<#�<$�<#��<#�e<#�<#؄<#�{<#��<#�D<#�<#��<#�J<#�8<#�<#�i<#��<#�<#�&<#�<#��<#�+<#��<#�D<#ף<#�8<#ڑ<#؄<#�<#�]<#ا<#�C<#ٛ<#׎<#�<#ڑ<#�<<#�<#��<#�I<#׎<#�"<#��<#��<#�C<#ף<#�8<#��<$	�<#��<#��<#ۮ<$"2<#��<#�l<#�J<#�<#�J<#�<#ٛ<#�<#�<#��<#��<#��<#�g<#�H<#�!<#��<#�<#��<#�<#�r<#�*<#�<#��<#��<#��<#؄<#��<#�5<#�<#��<#�<#�<#ߜ<#��<#�g<#�4<#�<#�&<#�<#�o<#ٛ<#�&<#��<#��<#��<#�5<#�l<#��<#�<#�&<#��<#�8<#�<#�<#�N<#��<#�e<#�r<#ܯ<#�<#�^<#ף<#��<#��<#��<#��<#ߜ<#�i<#�r<#�r<#�+<#�r<#�<#׎<#�U<#��<#��<#�<$�<#�<#�<#ܯ<#�<$Z<$�<$<<#�<#�<#�c<#ޫ<#�<#�N<#�<#�<$
�<$	<#�e<#ٛ<#�E<#��<$<<#��<#�<#�D<#�<#�o<#�<#�<#׎<#��<#�]<#�D<#�$<#��<#�e<#�J<#�J<#��<#�l<#�r<#�<#�<#��<#�<#�i<#�<#��<#�a<#�l<#�o<#�D<#�!<#��<#�i<#��<#�{<#�<#�i<#�{<#�X<#׎<#�r<#�	<$
�<$|d<$4e<#�8<#�<#�
<#�<#�8<#�C<#�N<#�<#��<$"2<$r<#�<#�a<#��<#׎<#�r<#��<#�<#�H<#�<#��<$p<#�l<#�4<#�&<#�<#�^<#��<#ޫ<#�E<#�4<#�&<#�"<#�D<#ף<#�<#��<#��<#�H<#�<#�*<#�<#�e<#��<#��<#׎<#��<#�<#�<#�m<$<<#�J<#ף<#�{<#�{<#�{<#��<#ߜ<$�<#�^<#ף<#ף<#�M<#ۮ<#�<#�E<#�<$F<$<<#ף<#�<#��<#��<#�<#�r<#��<#�4<#��<#�*<#�4<#�o<#��<#�<#ٛ<#��<#�J<#׎<#�<#��<#ܯ<$�<$L<#�l<#�r<#�<#�^<#�<#�"<#��<#�<#��<#�<#�l<#�e<#�o<#��<#ۮ<#ۮ<#��<#�4<#��<#�<#��<#�&<#�{<#�i<#�<#�i<#��<#�D<#�^<#�D<#��<#�5<#�<#��<#�D<#�+<#�<#��<#�g<#��<#�<#��<#�M<#�<#��<#��<#��<#�U<#�5<#�J<#׎<#�l<#�r<#ߜ<#�J<#��<#�)<#��<#��<#��<#�<#�<#�<#�E<#�&<#�<#��<#�o<#ף<#��<#׎<#׎<#�<#ڑ<#�N<#�<#�l<#�r<#�<#ٛ<#�<#��<#�r<#��<#�o<#�D<#�D<#�D<#��<#��<#ۮ<#�N<#�<#�J<#�*<#�J<#��<#��<#��<#ا<#�+<#�D<#��<#�r<#�<#�	<#��<#�<#�<#�<#��<#ף<#ۮ<#�<#��<#�8<#��<#ף<#�i<#��<#ٛ<#��<#�"<#�M<#��<#�^<#ۮ<#ا<#�
<#�<#ۮ<#��<#��<#��<#�J<#��<#��<#�r<#ף<#ٛ<#�<#�o<#��<#�<#�!<#ߜ<#�{<#�<#ۮ<#�<#�E<#�<#ۮ<#ٛ<#�o<#�<#�M<#�<#�+<#ۮ<#�<#��<#ۮ<#ۮ<#��<#�{<#��<#�r<#�X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = CTM_ADJ_PSAL, multiplicative adjustment term r = 1, no additional adjustment necessary.                                                                                                                                                              PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = PSAL, multiplicative adjustment term r = 1, no additional adjustment necessary.                                                                                                                                                                      None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            CTM: alpha=0.141C, tau=6.89s, rise rate = 10 cm/s with error equal to the adjustment;OW: r =1(+/-0.0001), vertically averaged dS =0.002(+/-0.002),                                                                                                              None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            OW: r =1(+/-0.0001), vertically averaged dS =0.002(+/-0.002),                                                                                                                                                                                                   SOLO-W floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface.  Additional correction was unnecessary in DMQC;      PRES_ADJ_ERR: SBE sensor accuracy + resolution error                                                   No significant temperature drift detected;  TEMP_ADJ_ERR: SBE sensor accuracy + resolution error                                                                                                                                                                PSAL_ADJ corrects Conductivity Thermal Mass (CTM), Johnson et al., 2007, JAOT.; No significant drift detected in conductivity                                                                                                                                   SOLO-W floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface.  Additional correction was unnecessary in DMQC;      PRES_ADJ_ERR: SBE sensor accuracy + resolution error                                                   No significant temperature drift detected;  TEMP_ADJ_ERR: SBE sensor accuracy + resolution error                                                                                                                                                                No thermal mass adjustment on non-primary profiles.; No significant drift detected in conductivity                                                                                                                                                              202203010000002022030100000020220301000000202203010000002022030100000020220301000000AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2020041122004520200411220045QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2020041122004520200411220045QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               WHOIWHOIARSQARSQWHQCWHQCV0.5V0.5                                                                                                                                2022030100000020220301000000QC  QC                                  G�O�G�O�G�O�G�O�G�O�G�O�                                WHOIWHOIARSQARSQCTM CTM V1.0V1.0                                                                                                                                2022030100000020220301000000IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                WHOIWHOIARCAARCAOWC OWC V2.0V2.0ARGO_for_DMQC_2021V03; CTD_for_DMQC_2021V02                     ARGO_for_DMQC_2021V03; CTD_for_DMQC_2021V02                     2022030100000020220301000000IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                