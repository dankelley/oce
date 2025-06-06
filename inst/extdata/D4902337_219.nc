CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       $Woods Hole Oceanographic Institution   source        
Argo float     history       92021-07-01T19:01:57Z creation; 2024-01-25T15:35:43Z DMQC;      
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
resolution        =���   axis      Z        �  <�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  LT   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  P@   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  _�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  c�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  s|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �$   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �L   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ͈   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �t   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �|   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �|   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �|   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �|   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  �  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �p   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                     ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                     ��Argo profile    3.1 1.2 19500101000000  20210701190157  20240125103543  4902337 4902337 US ARGO PROJECT                                                 US ARGO PROJECT                                                 BRECK OWENS, STEVEN JAYNE, P.E. ROBBINS                         BRECK OWENS, STEVEN JAYNE, P.E. ROBBINS                         PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6736                            6736                            2C  2C  DD  S2A                             S2A                             7373                            7373                            SBE602 ARM_v2.0_xmsg_ve         SBE602 ARM_v2.0_xmsg_ve         854 854 @�~B�1�>@�~B�1�>11  @�~B���`@�~B���`@F �@��@F �@���K��1Q�K��1Q11  GPS     GPS     Primary sampling: averaged [nominal 2 dbar binned data sampled at 0.5 Hz from a SBE41CP]                                                                                                                                                                        Near-surface sampling: discrete, pumped [data sampled at 1.0Hz from the same SBE41CP]                                                                                                                                                                              	   	AA  AA  AA  ?��?��H@:�H@}p�@��R@��R@�  @��RA  A#�
A@  A`  A�  A�  A�  A�  A�  A�  A߮A�  B (�B(�B(�B  B   B(  B0  B7�
B@  BG33BO�
BW�
B_�Bh  Bp  Bx  B�  B�  B�  B��
B�B��B�{B�=qB�{B�{B��B��B�{B�  B��
B��
B��B��B��
B��B��B�  B��B��B��B�  B�(�B�{B�  B�(�B�{B�  C 
=C  C
=C�C
=C	�C  C
=C  C��C��C��C
=C{C  C��C 
=C"
=C$  C%��C'��C*
=C+��C-��C0
=C2  C3��C6
=C8  C9��C<  C=��C@  CB  CD  CF  CH
=CI�CL  CN  CO�CR  CT{CV
=CW��CZ  C\
=C]�C`  Cb
=Cc��Ce�Cg�HCi�HCk��Cn
=Cp
=Cq��Cs��Cu�Cx  Cy��C|  C~{C�C�  C�  C�  C���C�  C���C���C�C�C���C���C�
=C�C�  C���C���C���C���C���C�  C�C���C�  C�
=C�C���C���C�  C���C���C���C�C���C�  C�  C�  C�
=C�
=C���C���C���C���C�C�C�
=C�C���C�C�\C�C�  C�
=C�C�  C�C�  C���C���C�  C�
=C�\C�
=C�  C���C�C���C���C���C�  C�C�  C���C�  C�
=C�C���C�
=C�  C��C���C�C�  C���C�  C�C�\C�C���C���C���C���C���C���C���C�  C�C�
=C�  C���C�C�  C���C�  C�C���C��C���C���C�  C�  C�  C�C�C�  C�  C�  C�C�C�C�C�
=C�C�  C�C�
=C�C���C���D � D  D}qD�qDz�D�qD��D  D� D�D��D�qD� D�D}qD�D}qD�qD	� D
�D
}qD
�RD}qD�qD}qD�qD� D�D� D��D}qD�D��D�qD� D�D}qD�D�D  Dz�D��Dz�D  D� D�D��D  D� D  D� D  D� D�qDz�D�qD� D�D� D��Dz�D�qD� D   D � D!  D!}qD"  D"� D#  D#�D$�D$� D$�qD%z�D%��D&}qD&�qD'}qD'�qD(}qD(�qD)� D*�D*��D*�qD+}qD+�qD,� D-  D-� D-�qD.}qD.�qD/� D0�D0��D1  D1xRD1��D2z�D2��D3xRD3�qD4� D4�qD5��D6D6�D7  D7� D8D8�D9  D9� D9�qD:}qD:�qD;}qD;�qD<}qD=�D=}qD>  D>� D>�qD?� D@�D@}qDA  DA�DB  DB}qDC�DC��DD  DD��DE�DE��DFDF�DGDG�DH�DH��DIDI��DJ�DJ}qDKDK��DL�DL� DL��DM� DN  DN}qDO  DO}qDO�qDP}qDQ  DQ� DQ�qDR�DSDS� DS��DT� DU  DU��DV�DV� DW  DWz�DX  DX�DY�DY}qDY�qDZ� DZ��D[� D\D\� D\�qD]}qD^�D^�D_  D_}qD_�qD`� Da  Da� Db  Db��Dc�Dc� Dd  Dd� De�De�Df  Dfz�Dg  Dg}qDg�qDh��Dh�qDiz�Dj  Dj�Dk  Dk}qDl�Dl� Dl��Dm}qDm�qDn� Dn�qDoz�Do�qDp� Dp�qDqz�Dr  Dr�Ds�Ds� Ds�qDtz�Du�Du� Dv  Dv�Dw�Dw��Dx
=?#�
?8Q�?aG�?��?�z�?���?�Q�?Ǯ?�
=?�@   @�@\)@
=@!G�@+�@333@=p�@E�@O\)@W
=@aG�@h��@s33@}p�@��\@��@��@���@�z�@�Q�@�(�@�G�@��@���@���@���@�z�@�Q�@�(�@�G�@��@���@���@��@�z�@�Q�@�(�@�  @��
@�@�=q@���@��@�z�@�Q�@��H@�p�A ��A�A�
A�A
=A��A
�HA(�Ap�A\)A��A�A�
AA
=A��A=qA(�Ap�A\)A ��A"�\A#�
A%A'
=A(Q�A*=qA+�A-p�A.�RA0��A1�A3�
A5�A7
=A8Q�A:=qA<(�A=p�A>�RA@��AB�\AC�
AEAG
=AH��AJ=qAL(�AN{AP  AQG�AR�\ATz�AUAXQ�AY��AZ�HA\��A^{A`  Aa�Ac33Ae�Ag
=AhQ�Aj�HAl(�An{Ap  AqG�As33Au�Aw
=Ax��Az�HA|(�A~{A�  A���A�G�A�=qA�33A��
A���A�A�ffA�\)A�Q�A�G�A��A�33A��
A�z�A�p�A�{A��RA�\)A�  A���A���A��\A�33A�(�A���A�p�A�{A��RA�\)A�  A���A���A��\A�33A�(�A���A�p�A�{A��RA�\)A�  A���A�G�A�=qA��HA��
A�z�A��A�A�ffA�
=A��A�Q�A���A���A��\A�33A��
A���A�p�A�ffA�
=A��A�Q�A�G�A��A��\A�33A��
A���A�p�A�ffA�
=A�  A���A�G�A�=qA��HA��
A�z�A��A�A�ffA�
=A�  A���A�G�A�=qA�33A�(�A��A�A�ffA�\)AǮAȣ�A�G�A�=qA��HA��
A�z�A�p�A�ffA�
=A�  A���Aљ�Aҏ\AӅA�(�A��A�AָRA�\)A�  A���Aٙ�Aڏ\AۅA�(�A��A�{A޸RA߮A��A�G�A�=qA�33A��
A���A�A�ffA�\)A�Q�A�G�A�=qA�33A�(�A���A�A�ffA�\)A�  A���A�A��HA��
A�z�A�p�A�{A�
=A�  A���A��A��\A��A�(�A��A�{A��RA��B (�B ��B�Bp�B�B=qB�RB33B�B  Bz�B��BG�BB=qB�RB33B�B  Bz�B��B	G�B	B
=qB
�\B
=B�B  BQ�B��BG�B��B�BffB�RB33B�B(�B��B�B��B�B=qB�RB
=B�B�
B(�B��B�Bp�B�B=qB�RB
=B�B  Bz�B��BG�BB=qB�RB
=B�B�
BQ�B��B�B��B�BffB�RB33B�B   B Q�B ��B!G�B!��B"{B"�RB#
=B#�B$  B$z�B$��B%p�B%B&=qB&�RB'33B'�B(  B(z�B(��B)p�B)�B*=qB*�HB+\)B+�
B,Q�B,��B-G�B-B.ffB.�HB/\)B/�
B0Q�B0��B1G�B1B2=qB2�RB333B3�B4(�B4��B5�B5��B5�B6ffB6�HB7\)B7�
B8Q�B8��B9G�B9B:{B:�RB;
=B;�B<  B<z�B<��B=p�B=�B>ffB>�RB?\)B?�B@(�B@��BA�BA��BB{BB�\BC
=BC�BC�
BDQ�BD��BEG�BEBF=qBF�RBG
=BG�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                            ?��?��H@:�H@}p�@��R@��R@�  @��RA  A#�
A@  A`  A�  A�  A�  A�  A�  A�  A߮A�  B (�B(�B(�B  B   B(  B0  B7�
B@  BG33BO�
BW�
B_�Bh  Bp  Bx  B�  B�  B�  B��
B�B��B�{B�=qB�{B�{B��B��B�{B�  B��
B��
B��B��B��
B��B��B�  B��B��B��B�  B�(�B�{B�  B�(�B�{B�  C 
=C  C
=C�C
=C	�C  C
=C  C��C��C��C
=C{C  C��C 
=C"
=C$  C%��C'��C*
=C+��C-��C0
=C2  C3��C6
=C8  C9��C<  C=��C@  CB  CD  CF  CH
=CI�CL  CN  CO�CR  CT{CV
=CW��CZ  C\
=C]�C`  Cb
=Cc��Ce�Cg�HCi�HCk��Cn
=Cp
=Cq��Cs��Cu�Cx  Cy��C|  C~{C�C�  C�  C�  C���C�  C���C���C�C�C���C���C�
=C�C�  C���C���C���C���C���C�  C�C���C�  C�
=C�C���C���C�  C���C���C���C�C���C�  C�  C�  C�
=C�
=C���C���C���C���C�C�C�
=C�C���C�C�\C�C�  C�
=C�C�  C�C�  C���C���C�  C�
=C�\C�
=C�  C���C�C���C���C���C�  C�C�  C���C�  C�
=C�C���C�
=C�  C��C���C�C�  C���C�  C�C�\C�C���C���C���C���C���C���C���C�  C�C�
=C�  C���C�C�  C���C�  C�C���C��C���C���C�  C�  C�  C�C�C�  C�  C�  C�C�C�C�C�
=C�C�  C�C�
=C�C���C���D � D  D}qD�qDz�D�qD��D  D� D�D��D�qD� D�D}qD�D}qD�qD	� D
�D
}qD
�RD}qD�qD}qD�qD� D�D� D��D}qD�D��D�qD� D�D}qD�D�D  Dz�D��Dz�D  D� D�D��D  D� D  D� D  D� D�qDz�D�qD� D�D� D��Dz�D�qD� D   D � D!  D!}qD"  D"� D#  D#�D$�D$� D$�qD%z�D%��D&}qD&�qD'}qD'�qD(}qD(�qD)� D*�D*��D*�qD+}qD+�qD,� D-  D-� D-�qD.}qD.�qD/� D0�D0��D1  D1xRD1��D2z�D2��D3xRD3�qD4� D4�qD5��D6D6�D7  D7� D8D8�D9  D9� D9�qD:}qD:�qD;}qD;�qD<}qD=�D=}qD>  D>� D>�qD?� D@�D@}qDA  DA�DB  DB}qDC�DC��DD  DD��DE�DE��DFDF�DGDG�DH�DH��DIDI��DJ�DJ}qDKDK��DL�DL� DL��DM� DN  DN}qDO  DO}qDO�qDP}qDQ  DQ� DQ�qDR�DSDS� DS��DT� DU  DU��DV�DV� DW  DWz�DX  DX�DY�DY}qDY�qDZ� DZ��D[� D\D\� D\�qD]}qD^�D^�D_  D_}qD_�qD`� Da  Da� Db  Db��Dc�Dc� Dd  Dd� De�De�Df  Dfz�Dg  Dg}qDg�qDh��Dh�qDiz�Dj  Dj�Dk  Dk}qDl�Dl� Dl��Dm}qDm�qDn� Dn�qDoz�Do�qDp� Dp�qDqz�Dr  Dr�Ds�Ds� Ds�qDtz�Du�Du� Dv  Dv�Dw�Dw��Dx
=?#�
?8Q�?aG�?��?�z�?���?�Q�?Ǯ?�
=?�@   @�@\)@
=@!G�@+�@333@=p�@E�@O\)@W
=@aG�@h��@s33@}p�@��\@��@��@���@�z�@�Q�@�(�@�G�@��@���@���@���@�z�@�Q�@�(�@�G�@��@���@���@��@�z�@�Q�@�(�@�  @��
@�@�=q@���@��@�z�@�Q�@��H@�p�A ��A�A�
A�A
=A��A
�HA(�Ap�A\)A��A�A�
AA
=A��A=qA(�Ap�A\)A ��A"�\A#�
A%A'
=A(Q�A*=qA+�A-p�A.�RA0��A1�A3�
A5�A7
=A8Q�A:=qA<(�A=p�A>�RA@��AB�\AC�
AEAG
=AH��AJ=qAL(�AN{AP  AQG�AR�\ATz�AUAXQ�AY��AZ�HA\��A^{A`  Aa�Ac33Ae�Ag
=AhQ�Aj�HAl(�An{Ap  AqG�As33Au�Aw
=Ax��Az�HA|(�A~{A�  A���A�G�A�=qA�33A��
A���A�A�ffA�\)A�Q�A�G�A��A�33A��
A�z�A�p�A�{A��RA�\)A�  A���A���A��\A�33A�(�A���A�p�A�{A��RA�\)A�  A���A���A��\A�33A�(�A���A�p�A�{A��RA�\)A�  A���A�G�A�=qA��HA��
A�z�A��A�A�ffA�
=A��A�Q�A���A���A��\A�33A��
A���A�p�A�ffA�
=A��A�Q�A�G�A��A��\A�33A��
A���A�p�A�ffA�
=A�  A���A�G�A�=qA��HA��
A�z�A��A�A�ffA�
=A�  A���A�G�A�=qA�33A�(�A��A�A�ffA�\)AǮAȣ�A�G�A�=qA��HA��
A�z�A�p�A�ffA�
=A�  A���Aљ�Aҏ\AӅA�(�A��A�AָRA�\)A�  A���Aٙ�Aڏ\AۅA�(�A��A�{A޸RA߮A��A�G�A�=qA�33A��
A���A�A�ffA�\)A�Q�A�G�A�=qA�33A�(�A���A�A�ffA�\)A�  A���A�A��HA��
A�z�A�p�A�{A�
=A�  A���A��A��\A��A�(�A��A�{A��RA��B (�B ��B�Bp�B�B=qB�RB33B�B  Bz�B��BG�BB=qB�RB33B�B  Bz�B��B	G�B	B
=qB
�\B
=B�B  BQ�B��BG�B��B�BffB�RB33B�B(�B��B�B��B�B=qB�RB
=B�B�
B(�B��B�Bp�B�B=qB�RB
=B�B  Bz�B��BG�BB=qB�RB
=B�B�
BQ�B��B�B��B�BffB�RB33B�B   B Q�B ��B!G�B!��B"{B"�RB#
=B#�B$  B$z�B$��B%p�B%B&=qB&�RB'33B'�B(  B(z�B(��B)p�B)�B*=qB*�HB+\)B+�
B,Q�B,��B-G�B-B.ffB.�HB/\)B/�
B0Q�B0��B1G�B1B2=qB2�RB333B3�B4(�B4��B5�B5��B5�B6ffB6�HB7\)B7�
B8Q�B8��B9G�B9B:{B:�RB;
=B;�B<  B<z�B<��B=p�B=�B>ffB>�RB?\)B?�B@(�B@��BA�BA��BB{BB�\BC
=BC�BC�
BDQ�BD��BEG�BEBF=qBF�RBG
=BG�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                            @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A;�A5�A-�-A+�A(�`A(A'C�A&��A%�A" �A��A�HA��A�FAv�@�Z@�J@��@�%@�hs@�5?@�9X@��/@�V@��!@x  @Z�@E�h@;��@9x�@+S�@=q?��P?�K�?�Ĝ?��#?�v�?�"�?�ȴ?�1?�Ĝ?�S�?ؓu?�M�?θR?�33?߾w?�ff@	��@&��@:M�@L(�@Xb@]�@mV@�o@��T@���@��@���@�ƨ@��@��
@�\)@�K�@ǶF@��@�;d@�r�@�ff@�bN@�ȴ@ݙ�@��`@�Z@�@�"�@�I�@܋D@�5?@ڗ�@睲@�A�A��A
^5AA�AE�A�TA1A+A�PA��A~�A|�A�A�TA�A �A��A-A(�A�uAVA
=A�\A��A�A~�Al�A�RA9XAE�A1'A�AVA�!A%At�A|�AI�A?}AJAS�A�
A��AȴAz�A �A��A"�Ar�A�hA�A�`A�+A$�AbA  AM�A�AƨA�A
{A	33A
r�AVA��A��AAJA��A��A��A��A(�AoA�RA��A5?A�hA M�@���@�ff@��H@��@��@��y@��@�33@�\)@�l�@�dZ@�\)@�K�@�33@��@�@��y@�V@���@���@�{@�p�@���@�I�@�  @���@�^5@�@��@�{@�O�@��`@�A�@�\)@��@�X@���@�(�@���@�9X@�F@�@�+@��@���@�V@�@�P@��@���@�@�Z@�;d@ݲ-@���@�K�@�"�@�ȴ@��@ّh@�V@ؓu@׶F@�@ָR@�n�@�$�@���@Չ7@�dZ@�~�@�^5@�X@�r�@ΰ!@�-@��#@̴9@�(�@�ƨ@�33@�{@�`B@ȼj@�(�@ǶF@�C�@���@�=q@�J@�{@�p�@ēu@�1@ÍP@�33@��@���@��`@�I�@��H@�bN@�S�@���@���@�E�@���@�hs@��@��@�bN@�A�@�1@��F@�dZ@�ȴ@�v�@�=q@�5?@�5?@�-@�{@��T@�hs@� �@��@��@�M�@��#@��^@���@��h@�x�@��@��u@�1'@�  @��w@�dZ@�C�@�33@��@��@�^5@�E�@�$�@��#@���@�G�@���@��@�b@�ƨ@��P@��!@���@��9@� �@��w@��@��\@�V@�-@�J@�@���@���@�x�@�X@���@��h@��`@�I�@��;@���@�t�@�33@�+@�o@���@��R@�C�@��F@�S�@��@�+@��@�
=@���@��+@�ff@�-@�{@�@��`@���@�r�@�A�@��`@�Q�@��@�A�@��u@�%@���@��@�ƨ@�l�@�I�@��@�X@�x�@�p�@���@���@��R@���@�~�@�E�@�J@��-@�`B@�&�@�Ĝ@���@�j@�A�@���@�ƨ@��P@�l�@�C�@���@���@��\@�$�@�`B@��@��
@��P@�o@���@��\@��@�x�@��h@�X@���@�@��#@���@�p�@�-@��R@���@��R@�v�@�M�@�J@��#@��-@���@���@��h@��@��@��@��@�p�@�`B@��@���@��@��`@��/@���@���@��j@���@��@�r�@�j@�Z@�Z@�A�@�(�@�b@���@��;@��w@��@�S�@�33@��y@�~�@�v�@�ff@�=q@��@�@�x�@�?}@�?}@�G�@�7L@��@�V@�%@�%@���@���@��@��`@��`@��/@���@���@���@�Ĝ@���@��@�Q�@�9X@���@���@�ƨ@��w@��F@��F@��F@��@��@�C�@�+@�o@��H@��!@�v�@�VA;
=A;VA;�A;�A;�A;"�A;+A;VA:�A8 �A333A1��A1�A0�A/C�A.v�A-�-A-`BA-/A-oA,ĜA,ffA,A+|�A*��A*��A*E�A*  A)��A)��A)C�A(�A(��A(jA(E�A(1'A($�A(�A({A(1A(A'��A'�mA'�;A'�wA'�7A'`BA'G�A'/A'"�A'�A'%A&��A&�`A&��A&��A&�!A&��A&��A&�DA&z�A&bNA&I�A&A�A&1'A& �A&  A%�-A%dZA%�A$�HA$�jA$��A$E�A#�hA#G�A"��A"Q�A"{A"1A!�#A!��A!�A!x�A!p�A!33A!?}A!O�A!�A!"�A!"�A!"�A!oA ��A ��A �A 5?A�A��A7LA�yA��A��Av�AE�A{A�FA�A�yAz�AZAbNA�mA33A��A�7AffA=qA-A�AhsA~�A-AA��Ap�A%A~�A��AVA��AȴA�jA�9A�!A�!A�A��A��A�\A�DA~�Av�An�Ar�An�AjA^5AI�A1'A�TA��AA�!A^5AQ�A�TAG�A�!AbA�^Al�A��A�DAE�A��A�A
��A
jA
5?A	��A�uAbA�uA1'A��A%A;dAK�@�~�@��D@�\)@��#@�(�@��@���@��u@�"�@��@��@�+@�@��@�V@�dZ@�o@�J@�`B@�Ĝ@��;@�K�@���@�v�@�hs@܋D@�"�@�M�@��@�+@�r�@�l�@�$�@�%@У�@�Z@��@Ϯ@�dZ@�"�@��y@���@Η�@�ff@�-@���@͉7@���@�1'@�C�@�$�@�&�@ȃ@� �@ǝ�@�
=@�n�@��@š�@�`B@��@��`@���@ļj@ě�@�z�@�A�@�1@þw@ÍP@�\)@�@���@���@�b@���@�l�@�S�@�;d@�
=@��\@��@���@��@���@�33@�n�@�p�@��@��@��F@�33@�ȴ@�~�@�E�@�5?@��@�J@�@���@��@�p�@�%@��/@�Ĝ@���@�z�@�b@�t�@��@��y@���@�E�@�=q@�5?@�-@�-@�-@�-@�-@�$�@�$�@�$�@��@�$�@�{@�{@��@��@�{@���@��h@�%@��/@���@�z�@�bN@�I�@�A�@�  @��F@�dZ@�S�@�33@�+@�@��@��!@�n�@�$�@��@�@���@�`B@�/@��@�%@��`@��9@�A�@�(�@��m@��@�l�@�;d@��@��y@��R@��-@�G�@��/@�Q�@�  @��
@�dZ@�
=@���@�-@���@�x�@���@�(�@���@�b@��D@�(�@�V@�+@�$�@�Ĝ@�K�@���@�j@���@�&�@�33@�-@�hs@���@��u@�z�@�9X@�C�@�v�@���@�&�@���@�Q�@l�@~v�@}�@{ƨ@{33@zn�@x�9@vV@r��@lj@h�9@g
=@ep�@co@`�@^��@^$�@]��@]`B@[�
@Z^5@Yx�@Y&�@Y&�@Y%@XĜ@W�w@Up�@Rn�@P1'@N�R@M?}@L(�@J�!@HĜ@F�+@D�@CC�@BM�@A�#@A�7@@��@@A�@?�P@?�@>ȴ@>E�@=��@=`B@<�@<j@<1@;ƨ@;�@;C�@;"�@;o@:�@:�\@:M�@:M�@:=q@:-@9��@9�#@9��@9��@9��@9X@9X@9hs@97LG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                            A;�A5�A-�-A+�A(�`A(A'C�A&��A%�A" �A��A�HA��A�FAv�@�Z@�J@��@�%@�hs@�5?@�9X@��/@�V@��!@x  @Z�@E�h@;��@9x�@+S�@=q?��P?�K�?�Ĝ?��#?�v�?�"�?�ȴ?�1?�Ĝ?�S�?ؓu?�M�?θR?�33?߾w?�ff@	��@&��@:M�@L(�@Xb@]�@mV@�o@��T@���@��@���@�ƨ@��@��
@�\)@�K�@ǶF@��@�;d@�r�@�ff@�bN@�ȴ@ݙ�@��`@�Z@�@�"�@�I�@܋D@�5?@ڗ�@睲@�A�A��A
^5AA�AE�A�TA1A+A�PA��A~�A|�A�A�TA�A �A��A-A(�A�uAVA
=A�\A��A�A~�Al�A�RA9XAE�A1'A�AVA�!A%At�A|�AI�A?}AJAS�A�
A��AȴAz�A �A��A"�Ar�A�hA�A�`A�+A$�AbA  AM�A�AƨA�A
{A	33A
r�AVA��A��AAJA��A��A��A��A(�AoA�RA��A5?A�hA M�@���@�ff@��H@��@��@��y@��@�33@�\)@�l�@�dZ@�\)@�K�@�33@��@�@��y@�V@���@���@�{@�p�@���@�I�@�  @���@�^5@�@��@�{@�O�@��`@�A�@�\)@��@�X@���@�(�@���@�9X@�F@�@�+@��@���@�V@�@�P@��@���@�@�Z@�;d@ݲ-@���@�K�@�"�@�ȴ@��@ّh@�V@ؓu@׶F@�@ָR@�n�@�$�@���@Չ7@�dZ@�~�@�^5@�X@�r�@ΰ!@�-@��#@̴9@�(�@�ƨ@�33@�{@�`B@ȼj@�(�@ǶF@�C�@���@�=q@�J@�{@�p�@ēu@�1@ÍP@�33@��@���@��`@�I�@��H@�bN@�S�@���@���@�E�@���@�hs@��@��@�bN@�A�@�1@��F@�dZ@�ȴ@�v�@�=q@�5?@�5?@�-@�{@��T@�hs@� �@��@��@�M�@��#@��^@���@��h@�x�@��@��u@�1'@�  @��w@�dZ@�C�@�33@��@��@�^5@�E�@�$�@��#@���@�G�@���@��@�b@�ƨ@��P@��!@���@��9@� �@��w@��@��\@�V@�-@�J@�@���@���@�x�@�X@���@��h@��`@�I�@��;@���@�t�@�33@�+@�o@���@��R@�C�@��F@�S�@��@�+@��@�
=@���@��+@�ff@�-@�{@�@��`@���@�r�@�A�@��`@�Q�@��@�A�@��u@�%@���@��@�ƨ@�l�@�I�@��@�X@�x�@�p�@���@���@��R@���@�~�@�E�@�J@��-@�`B@�&�@�Ĝ@���@�j@�A�@���@�ƨ@��P@�l�@�C�@���@���@��\@�$�@�`B@��@��
@��P@�o@���@��\@��@�x�@��h@�X@���@�@��#@���@�p�@�-@��R@���@��R@�v�@�M�@�J@��#@��-@���@���@��h@��@��@��@��@�p�@�`B@��@���@��@��`@��/@���@���@��j@���@��@�r�@�j@�Z@�Z@�A�@�(�@�b@���@��;@��w@��@�S�@�33@��y@�~�@�v�@�ff@�=q@��@�@�x�@�?}@�?}@�G�@�7L@��@�V@�%@�%@���@���@��@��`@��`@��/@���@���@���@�Ĝ@���@��@�Q�@�9X@���@���@�ƨ@��w@��F@��F@��F@��@��@�C�@�+@�o@��H@��!@�v�@�VA;
=A;VA;�A;�A;�A;"�A;+A;VA:�A8 �A333A1��A1�A0�A/C�A.v�A-�-A-`BA-/A-oA,ĜA,ffA,A+|�A*��A*��A*E�A*  A)��A)��A)C�A(�A(��A(jA(E�A(1'A($�A(�A({A(1A(A'��A'�mA'�;A'�wA'�7A'`BA'G�A'/A'"�A'�A'%A&��A&�`A&��A&��A&�!A&��A&��A&�DA&z�A&bNA&I�A&A�A&1'A& �A&  A%�-A%dZA%�A$�HA$�jA$��A$E�A#�hA#G�A"��A"Q�A"{A"1A!�#A!��A!�A!x�A!p�A!33A!?}A!O�A!�A!"�A!"�A!"�A!oA ��A ��A �A 5?A�A��A7LA�yA��A��Av�AE�A{A�FA�A�yAz�AZAbNA�mA33A��A�7AffA=qA-A�AhsA~�A-AA��Ap�A%A~�A��AVA��AȴA�jA�9A�!A�!A�A��A��A�\A�DA~�Av�An�Ar�An�AjA^5AI�A1'A�TA��AA�!A^5AQ�A�TAG�A�!AbA�^Al�A��A�DAE�A��A�A
��A
jA
5?A	��A�uAbA�uA1'A��A%A;dAK�@�~�@��D@�\)@��#@�(�@��@���@��u@�"�@��@��@�+@�@��@�V@�dZ@�o@�J@�`B@�Ĝ@��;@�K�@���@�v�@�hs@܋D@�"�@�M�@��@�+@�r�@�l�@�$�@�%@У�@�Z@��@Ϯ@�dZ@�"�@��y@���@Η�@�ff@�-@���@͉7@���@�1'@�C�@�$�@�&�@ȃ@� �@ǝ�@�
=@�n�@��@š�@�`B@��@��`@���@ļj@ě�@�z�@�A�@�1@þw@ÍP@�\)@�@���@���@�b@���@�l�@�S�@�;d@�
=@��\@��@���@��@���@�33@�n�@�p�@��@��@��F@�33@�ȴ@�~�@�E�@�5?@��@�J@�@���@��@�p�@�%@��/@�Ĝ@���@�z�@�b@�t�@��@��y@���@�E�@�=q@�5?@�-@�-@�-@�-@�-@�$�@�$�@�$�@��@�$�@�{@�{@��@��@�{@���@��h@�%@��/@���@�z�@�bN@�I�@�A�@�  @��F@�dZ@�S�@�33@�+@�@��@��!@�n�@�$�@��@�@���@�`B@�/@��@�%@��`@��9@�A�@�(�@��m@��@�l�@�;d@��@��y@��R@��-@�G�@��/@�Q�@�  @��
@�dZ@�
=@���@�-@���@�x�@���@�(�@���@�b@��D@�(�@�V@�+@�$�@�Ĝ@�K�@���@�j@���@�&�@�33@�-@�hs@���@��u@�z�@�9X@�C�@�v�@���@�&�@���@�Q�@l�@~v�@}�@{ƨ@{33@zn�@x�9@vV@r��@lj@h�9@g
=@ep�@co@`�@^��@^$�@]��@]`B@[�
@Z^5@Yx�@Y&�@Y&�@Y%@XĜ@W�w@Up�@Rn�@P1'@N�R@M?}@L(�@J�!@HĜ@F�+@D�@CC�@BM�@A�#@A�7@@��@@A�@?�P@?�@>ȴ@>E�@=��@=`B@<�@<j@<1@;ƨ@;�@;C�@;"�@;o@:�@:�\@:M�@:M�@:=q@:-@9��@9�#@9��@9��@9��@9X@9X@9hs@97LG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                            ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A�/A�7LA�I�A�G�A�I�A�K�A�O�A�z�A���B B 6FB =qB ^5B ��B ��B �NB ��B
=BhBoB�B�B`BB�B�RB�sB��B��BJB33Bv�B�PB��B�B��B�B�B��B�B(�B>wB]/Bs�B�7B��B��BBF�Bq�B�B�B��B"�B�B�JBz�B��B�#BD�B��B1'BhsB�B�^B�B
=B49BhsB��B��B��BɺB�`B�mB�B�5B�B	1B	�B	l�B	ŢB
+B
�hB
��B
��B"�BA�BN�BI�B:^B+B<jB9XB~�B�{B��B�B�qBȴB��B�/B�B�B�B��B��B�B�fB�BB�`B�B��B��B�B&�B33B=qBJ�B`BBn�B~�Bl�B`BBZBXBVBT�BP�BL�BG�BA�BA�BE�BK�BP�BS�BW
B_;BbNBo�Bx�BcTB_;Bt�B�bB�{B�hB�Bn�BhsBiyBffBbNB\)BXBZB[#BZBW
BW
BO�B]/BiyBk�Bn�Br�Bs�Bw�Bz�B~�B�B�B�%B�+B�1B�1B�+B�%B�B�B�B�B� B~�B}�B|�B{�By�Bw�Bu�Bt�Bs�Br�Bp�Bo�Bn�Bm�Bk�BiyBgmBgmBgmBffBe`BdZBcTBbNBaHB`BB_;B]/B[#BZBXBVBVBT�BS�BS�BS�BR�BQ�BP�BO�BN�BN�BN�BM�BK�BI�BH�BG�BE�BD�BC�BB�BA�BA�B@�B@�B@�B?}B>wB=qB<jB<jB;dB:^B8RB7LB7LB5?B33B2-B1'B0!B.B,B(�B'�B$�B#�B"�B!�B!�B!�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{BuBoB\BDB+BBBB  B  B  B��B��B��B��B��BBBB  B��B��B��B��B��B��B��B��B��BB+B%BB%B%B%BBBBBBB��B��B��B��BBBBB%B1B1B%B%B%BDB\BuB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{BoB\BJBDB	7B1B+BBBBB%B	7B1B1B1BJBbB\B\B\B\B\B\B\B\B\B\BbBbBbBbBbBbBbBbB\B\B\B\B\B\B\B\B\BVBVBVBVBVBVBPBPBPBJBJBDBDB
=B
=B	7B	7B1B1B1B+B1B+B+B+B+B+B+B+B+B+B+B+B+B%B%B%B%B%B%B%BBBBBBBBBBBBBBBBBA���A���A���A���A���A��7A�~�A��A���A��/A�K�A�{A�&�A�33A�A�A�7LA�5?A�(�A�&�A�&�A�(�A�&�A�+A�33A�;dA�?}A�E�A�?}A�A�A�C�A�G�A�K�A�M�A�M�A�K�A�K�A�I�A�K�A�I�A�G�A�G�A�G�A�E�A�G�A�I�A�K�A�K�A�I�A�I�A�G�A�I�A�K�A�K�A�K�A�K�A�K�A�K�A�G�A�I�A�K�A�K�A�M�A�K�A�K�A�I�A�K�A�K�A�VA�Q�A�VA�VA�S�A�S�A�`BA�hsA�l�A�r�A��A�z�A�|�A��A��A��A��A��A��+A�~�A��A��A��A�|�A�~�A�|�A�~�A��A��DA��\A��uA���A���A���A���A��A��A��!A��!A�A�A�ĜA���A�ĜA���A���A��HA��TB JB 1B B B 1B hB �B �B �B �B �B �B '�B 49B 6FB 8RB 7LB 8RB 8RB 8RB 7LB 8RB 7LB 8RB 9XB 7LB 8RB 9XB 8RB 7LB 6FB 7LB 8RB 9XB ;dB C�B C�B A�B <jB ;dB :^B C�B @�B J�B ;dB 8RB 9XB @�B ;dB =qB I�B =qB G�B <jB ;dB <jB C�B G�B ZB I�B F�B P�B _;B t�B p�B q�B o�B y�B v�B w�B x�B }�B ~�B ~�B � B �=B �oB ��B �B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B �B ��B ��B ǮB ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B �
B �B �)B �5B �HB �NB �NB �TB �TB �TB �TB �TB �TB �TB �ZB �ZB �`B �ZB �TB �yB �B �B �B �B �B �B �B �B �B ��B ��B  B ��B ��B ��BBB%B+B
=B	7B
=B1B	7B	7B1B1B1B1BPBDB
=B
=B
=BDBVBhBhBbB�BoBuBoBoBoBoBhBhBhBbBbBbB\B\BVBVBPBPBVBhBuBhBhBoBhBhBhBuB{B{B{BuBuBuBuB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BuB�B �B�B�BuB�B�B�BuB{BuBoBVBPBVB�B.BG�BYB^5BW
BVBXBZBXBZB]/Be`Be`BcTBe`Be`BcTBbNBffBm�Bk�Bo�Bl�BiyBm�Bp�Bt�B|�B� B~�B}�B�7B�B��B�oB��B��B��B��B�'B�!B�!B�!B�-B�XB�qB�jB�^B�XB�RB�LB�qBƨB��B��B��B�B�B�)B�TB�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                            A��OA�8�A�H�A�KyA�R�A�O�A�R�A�Z`A�j�A���A���B ?B ?�B a�B ��B �NB ��B �-BYB�BhB�B!�B@Bv_B��B�sB�B�2B /B�BD�B|PB��B�/B��B�B��B��BFB�B'B;[Bd2BrB��B�B�8B�B6�BdB��B��B�B��B�6B�Bu�B��B��B4�Bz@B$�B\�B�eB��B�;BVB+JB`&B��B�ZB�0B��B�B�BނB��B��B	B	�B	XpB	��B
�B
�XB
��B
�B�BABQ^BN]B?B)�B>XB/�B|B�8B�SB��B��BȞBδB��B�B�B�2B�dB�$B��B�hB��B�]B�B��B��BiB%�B1�B=4BHdB]\Bl�B��BqBc BZ�BX�BWBV�BROBN�BJ@BB�BBBBF�BL�BQ BT BVXBb0B`�Bm�BxBe�B[{BoWB�]B��B�5B�ZBr
Bi�BlNBhgBd�B_<BY5BZ�B\\B\BZ�BZ�BOeB\~BiSBk�Bn�Br�BseBw�Bz�B B�B�3B�FB�OB�RB�aB�=B��B��B��B�B��B��BqB~�B~�B} B|�BzBv�BueBt�Bt$Br�Bp~BooBn�Bn�BlBh>Bg�Bg�Bf�Bf�Be�Bd>Bc�BbB`�B`�B_@B\�B\�BZ�BWBVKBU�BUBBT�BT�BS�BS:BQ�BPPBOJBOIBOVBNnBOBKBIBIHBGBG)BDxBC)BC2BBbBABArBB;B@�B?uB>ZB="B="B<*B;"B8�B7TB8?B6�B4B2�B1�B0�B0	B-4B)�B*:B(�B%�B#�B"$B"RB"KB!�B zB &B 2B�B B<B:B�B;BB�B�B�B�BBB�B�B�B~BPB�B�B�B�B2BbB6B�BB'B�B�B�B�BOB�B�BB�BBLB B4B�B�B�B�B�BB�BB�B ^B BB 3B�lB�!B��B�DB�,B �B,B!B �B��B�XB�/B�RB�B�!B��B��B�$BrB�B�B�B;BCB�BzBQBjB>B�BBB�4B�|B�4B�B�B_B �B�B�B�B	.B�B�B�B
�B`B9B�B�BQBtB�B0BBB;B2BB:B�B�B�BB�B�B�B�B�B�B�B0B�B�B�B�BB	�BcBB�BBbBdB�B	iB�BlBBzB>BqB�B�B�B�B�BjBtBdBrB^BaBdB|B~B�B�ByBiBgBgBiBvB�B�BvBhBrBWB~B|B{ByBzB�B�B�BB�B�B
TB
VB	{B	�BzB�B�B4BBHB]B8B5B)B7B7B7B7B,B5B5B&B%B@B[B\BqBLB�B[B*B$B#BBB&B[BrB8B5BTBZBeB8BA���A���A���A���A���A��7A�~�A��A���A��/A�K�A�{A�&�A�33A�A�A�7LA�5?A�(�A�&�A�&�A�(�A�&�A�+A�33A�;dA�?}A�E�A�?}A�A�A�C�A�G�A�K�A�M�A�M�A�K�A�K�A�I�A�K�A�I�A�G�A�G�A�G�A�E�A�G�A�I�A�K�A�K�A�I�A�I�A�G�A�I�A�K�A�K�A�K�A�K�A�K�A�K�A�G�A�I�A�K�A�K�A�M�A�K�A�K�A�I�A�K�A�K�A�VA�Q�A�VA�VA�S�A�S�A�`BA�hsA�l�A�r�A��A�z�A�|�A��A��A��A��A��A��+A�~�A��A��A��A�|�A�~�A�|�A�~�A��A��DA��\A��uA���A���A���A���A��A��A��!A��!A�A�A�ĜA���A�ĜA���A���A��HA��TB JB 1B B B 1B hB �B �B �B �B �B �B '�B 49B 6FB 8RB 7LB 8RB 8RB 8RB 7LB 8RB 7LB 8RB 9XB 7LB 8RB 9XB 8RB 7LB 6FB 7LB 8RB 9XB ;dB C�B C�B A�B <jB ;dB :^B C�B @�B J�B ;dB 8RB 9XB @�B ;dB =qB I�B =qB G�B <jB ;dB <jB C�B G�B ZB I�B F�B P�B _;B t�B p�B q�B o�B y�B v�B w�B x�B }�B ~�B ~�B � B �=B �oB ��B �B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B �B ��B ��B ǮB ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B �
B �B �)B �5B �HB �NB �NB �TB �TB �TB �TB �TB �TB �TB �ZB �ZB �`B �ZB �TB �yB �B �B �B �B �B �B �B �B �B ��B ��B  B ��B ��B ��BBB%B+B
=B	7B
=B1B	7B	7B1B1B1B1BPBDB
=B
=B
=BDBVBhBhBbB�BoBuBoBoBoBoBhBhBhBbBbBbB\B\BVBVBPBPBVBhBuBhBhBoBhBhBhBuB{B{B{BuBuBuBuB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BuB�B �B�B�BuB�B�B�BuB{BuBoBVBPBVB�B.BG�BYB^5BW
BVBXBZBXBZB]/Be`Be`BcTBe`Be`BcTBbNBffBm�Bk�Bo�Bl�BiyBm�Bp�Bt�B|�B� B~�B}�B�7B�B��B�oB��B��B��B��B�'B�!B�!B�!B�-B�XB�qB�jB�^B�XB�RB�LB�qBƨB��B��B��B�B�B�)B�TB�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                            = Ѻ=��<�'/<e��<3#�<0$<3-C<I3.<��<S\�<��?<�P�<`��=��=J�<ے�<��<z֔<j�<3d|<*��<7��<sfd=#1R<���<���<���<G�4<(Q'<iŀ<�D}<���<:/G<.#�<,�<%�J<$�<MN<2�o<1�<$��<&�*<+:<F��<%�~<8g<W��<x�3<�N(<�¯<�o<[:<8�"<�c=!��<GG<���<7pU<[	�<��<���= �<�͜<v�I<%�n<,�<ND�<N�><Ymj<R��<&�n<%��<4��<4�V<'a�<;k<2JJ<#�	<'a�<.�!<���<���<��><���<�*v<GP5<\ik<5>�<#�H<(��<3ڈ<46<%`�<&�H<a�b<*�F<#�<$v<#��<$�<#؄<$��<%B�<#�<%>�<$�<$�<A~<+Z�<&��<%��<#�<#�<*�<,�<$�q<$��<%<#�e<(%�<*F�<&v�<LO�<36�<)��<$3U<$��<$��<%��<%e<&��<(�)<%gB<$?[<$�w<$��<#�<#��<$7�<*��<&�<&h�<C=�<()+<.{<9/L<$�j<#�<4��<Rp<,�q<%*<)��<&�<(�(<+n<$�7<#��<% �<&�a</O�</��<$p<$6�<#�r<#�
<#�X<#�c<#�<#��<#ا<#�&<#�X<#�<#�]<#��<#�]<#�<$��<.�!<%`�<$�<$�<$x+<$b�<$<<$)
<&|V<$�R<+"�<'��<$��<$.<$�<%{@<'r#<$o�<$c�<$�(<+�^<(��<$\"<#�<$�<$�<%�<$��<$}�<%B�<$E<$�<%�6<'$�<&n4<(r_<(��<$��<#�l<$$<%"<$ K<$Sa<$MO<%*<$��<#��<#��<#�N<$�<$ K<+�<%B�<#�<%��<%gB<(��<$r�<$Z<%��<$f�<$k<$� <&!�<$�<$�w<$|d<$><<$><<$N�<$L<#�<#�<<$��<%<�<$r�<$B�<$�<$&<&�*<$��<$�<'�:</ �<&e<$U�<#�5<$�<$	<$n�<$B�<#�	<$ <#�E<#�<$.<$
�<$�V<$a<#�<#׎<#�<#׎<#ۮ<#�<$W<&�^<$��<$�<$ub<$<<#��<#�8<#ٛ<#�<$}<$P�<$!><#�!<#�	<$�<#�N<#ٛ<#ܯ<#�	<$B�<#�+<#�<#��<#�	<$	<$F9<$�<$8�<$�<#��<% <%B�<&�<$�t<$3U<$��<$f�<#��<#�U<#��<#��<#ۮ<#�<#�<#ޫ<#��<#�*<$��<$��<$0.<#�Q<#��<#��<#׎<#�8<$!><#��<$b�<$,<$�<$�<#�<#؄<#��<#�N<#�N<#��<#�<#ޫ<$�<%�<#�<$r<#��<$aD<$I�<#�5<#�&<$/<$(<#�m<$��<$�<$ <%MY<$(<$�e<#�<#�<$'<%F<#��<#�]<$�<#��<#�N<$G<$�<#�<$�<#��<#�!<#�<$ <#�<#�<#�N<#��<#��<#�m<#�<$4e<$�`<$�1<%&<$a<$E<$+<#ޫ<$��<$4e<#ا<#�M<$:�<#�M<#ޫ<#��<#�<$�`<$Z�<#��<#�c<#�<#�&<#��<#�!<#�<#ף<#��<#�<<#؄<#�<#�<#�<#�<#�o<#��<#�l<#ا<#׎<#�i<#�i<#׎<#�<#�^<#�r<#�<#�{<#�o<#�<#��<#�r<#�8<#��<#�l<#�E<#�<#�4<#ߜ<$ �<$!><#ا<#��<#�&<$p<#�M<#��<#�<#�I<#�<#ٛ<#ޫ<#׎<#�X<#�<#�{<#�{<#�{<#�{<#�<#�X<#�X<#�<#�
<#�D<#��<#�E<#�<#ۮ<#�W<#�<#��<#�i<#�X<#�<#�<#׎<#�U<#�(<#��<#�+<#��<#�<#�5<#ޫ<#�0<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = CTM_ADJ_PSAL, multiplicative adjustment term r = 1, no additional adjustment necessary.                                                                                                                                                              PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = PSAL, multiplicative adjustment term r = 1, no additional adjustment necessary.                                                                                                                                                                      None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            CTM: alpha=0.141C, tau=6.89s, rise rate = 10 cm/s with error equal to the adjustment;OW: r =1(+/-0), vertically averaged dS = 0(+/-0.001),                                                                                                                      None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            OW: r =1(+/-0), vertically averaged dS = 0(+/-0.001),                                                                                                                                                                                                           SOLO-W floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface.  Additional correction was unnecessary in DMQC;      PRES_ADJ_ERR: SBE sensor accuracy + resolution error                                                   No significant temperature drift detected;  TEMP_ADJ_ERR: SBE sensor accuracy + resolution error                                                                                                                                                                PSAL_ADJ corrects Conductivity Thermal Mass (CTM), Johnson et al., 2007, JAOT.; No significant drift detected in conductivity                                                                                                                                   SOLO-W floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface.  Additional correction was unnecessary in DMQC;      PRES_ADJ_ERR: SBE sensor accuracy + resolution error                                                   No significant temperature drift detected;  TEMP_ADJ_ERR: SBE sensor accuracy + resolution error                                                                                                                                                                No thermal mass adjustment on non-primary profiles.; No significant drift detected in conductivity                                                                                                                                                              202401250000002024012500000020240125000000202401250000002024012500000020240125000000AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021070119015720210701190157QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021070119015720210701190157QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               WHOIWHOIARSQARSQWHQCWHQCV0.5V0.5                                                                                                                                2022071100000020220711000000QC  QC                                  G�O�G�O�G�O�G�O�G�O�G�O�                                WHOIWHOIARSQARSQCTM CTM V1.0V1.0                                                                                                                                2024012416220120240124162201IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                WHOIWHOIARCAARCAOWC OWC V2.0V2.0ARGO_for_DMQC_2022V03; CTD_for_DMQC_2021V02                     ARGO_for_DMQC_2022V03; CTD_for_DMQC_2021V02                     2024012500000020240125000000IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                