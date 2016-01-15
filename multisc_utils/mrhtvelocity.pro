; docformat = 'rst'
;
; NAME:
;       MrHTVelocity
;
;*****************************************************************************************
;   Copyright (c) 2014, University of New Hampshire                                      ;
;   All rights reserved.                                                                 ;
;                                                                                        ;
;   Redistribution and use in source and binary forms, with or without modification,     ;
;   are permitted provided that the following conditions are met:                        ;
;                                                                                        ;
;       * Redistributions of source code must retain the above copyright notice,         ;
;         this list of conditions and the following disclaimer.                          ;
;       * Redistributions in binary form must reproduce the above copyright notice,      ;
;         this list of conditions and the following disclaimer in the documentation      ;
;         and/or other materials provided with the distribution.                         ;
;       * Neither the name of the University of New Hampshire nor the names of its       ;
;         contributors may be used to endorse or promote products derived from this      ;
;         software without specific prior written permission.                            ;
;                                                                                        ;
;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY  ;
;   EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES ;
;   OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT  ;
;   SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,       ;
;   INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED ;
;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR   ;
;   BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN     ;
;   CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN   ;
;   ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH  ;
;   DAMAGE.                                                                              ;
;*****************************************************************************************
;
; PURPOSE:
;+
;       Calculate the velocity of the deHoffmann-Teller frame
;       using the convective electric field as a proxy for the measured
;       electric field.
;
;
;   Sources:
;       Sonnerup, B. U. Ö., Papamastorakis, I., Lohr, H., Paschmann, G., & Lühr, H. (1987).
;           Magnetopause properties from AMPTE/IRM observations of the convection electric
;           field: Method development. Journal of Geophysical Research: Space Physics,
;           92(A11), 12137–12159. doi:10.1029/JA092iA11p12137
;
;       Daly, P. W., & Paschmann, G. (1998). Analysis Methods for Multi-spacecraft Data.
;           International Space Science Institute. Chp 9.
;
; :Examples:
;   An example is included at the end and is a main level program
;       IDL> .r MrHTVelocity
;
; :Params:
;       V:          in, required, type=3xN numeric
;                   Three components of the plasma velocity, in units of kilometers per
;                       per second (km/s)
;       B:          in, required, type=3xN numeric
;                   Three components of the magnetic field, in units of nanoTesla (nT),
;                       taken at the same times as `V`.
;       T:          in, optional, type=vector
;                   Time at which each measurement of `V` and `B` were taken. If present,
;                       the deHoffmann-Teller Frame will be assumed to have a constant
;                       acceleration: vHT = vHT_0 + aHT*t.
;
; :Keywords:
;       AHT:        out, optional, type=dblarr(3)
;                   A named variable into which the acceleration of the deHoffmann-Teller
;                       frame is returned. Units: km/s^2
;
; :Returns:
;       VHT:        Three components of the deHoffmann-Teller velocity. Units: km/s
;
; :History:
;   Modification History::
;       04/02/2012  -   Written by Matthew Argall
;       02/16/2012  -   Changed all operations do double precision. - MRA
;       2014/04/04  -   Added the ability to calculate accelerated HT frames. - MRA
;       2014/04/05  -   Vectorized arithmetic. Plot example data and results in example
;                           program at end of file. - MRA
;-
function MrHTVelocity, v, B, t, $
AHT=aHT
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        MrPrintF, 'LogErr'
        return, -1
    endif
;---------------------------------------------------------------------
; Check Parameters ///////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Make sure that v is Nx3:
    dimsV = size(v, /DIMENSIONS)
    if size(v, /N_DIMENSIONS) ne 2 then message, 'V must be a 3xN or Nx3 array.'
    if dimsV[0] ne 3 then begin
        if dimsV[1] eq 3 $
            then _v = v $
            else message, 'V must be a 3xN or Nx3 array.'
    endif else _v = transpose(v)
        
    ;Make sure that B is Nx3
    dimsB= size(B, /DIMENSIONS)
	if size(B, /N_DIMENSIONS) ne 2 then begin
	    if dimsB[1] eq 3 $
	        then _B = dobule(B) $
	        else message, 'B must be a 3xN or Nx3 array.'
	endif else _B = transpose(double(B))
	
	;Make sure v and B have the same number of elements
	if n_elements(v) ne n_elements(B) then message, 'v and B must have the same number of elements'
	
	;Total number of points
	npts = n_elements(_v[*,0])
	
	;Check the time.
	nT = n_elements(t)
	if nT gt 0 then if nT ne nPts then message, 'Incorrect number of elements: t.'
	
;---------------------------------------------------------------------
; Preparing Matrices /////////////////////////////////////////////////
;---------------------------------------------------------------------
	BiBj = dblarr(nPts, 3, 3)
	Kij  = dblarr(nPts, 3, 3)
	
	;|B|^2 delta_{i,j}
	BB = _B[*,0]*_B[*,0] + _B[*,1]*_B[*,1] + _B[*,2]*_B[*,2]
	
	;Outer product of vectors B and B: BiBj = Bi*Bj
	BiBj = rebin(reform(_B, npts, 1, 3), npts, 3, 3) * rebin(_B, npts, 3, 3)
        
    ;Projection matrix * B^2
    Kij = -temporary(BiBj)
    Kij[*,0,0] = BB + Kij[*,0,0]
    Kij[*,1,1] = BB + Kij[*,1,1]
    Kij[*,2,2] = BB + Kij[*,2,2]
    BB = 0B

    ;Average value of Kij
    K_0 = MrMean(Kij, DIMENSION=1)

    ;Calculate the mean of Kij * vi (the dot product in IDL is the outer product)
    Kv_0 = [ mean(Kij[*,0,0]*_v[*,0] + Kij[*,1,0]*_v[*,1] + Kij[*,2,0]*_v[*,2]), $
             mean(Kij[*,0,1]*_v[*,0] + Kij[*,1,1]*_v[*,1] + Kij[*,2,1]*_v[*,2]), $
             mean(Kij[*,0,2]*_v[*,0] + Kij[*,1,2]*_v[*,1] + Kij[*,2,2]*_v[*,2]) ]

;---------------------------------------------------------------------
; Constant Velocity Frame ////////////////////////////////////////////
;---------------------------------------------------------------------
    if n_elements(t) eq 0 then begin
        _v  = 0B
        Kij = 0B

        ;Compute the deHoffmann-Teller velocity
        vHT = invert(K_0) # Kv_0
        aHT = dblarr(3)
        
;---------------------------------------------------------------------
; Accelerated Frame //////////////////////////////////////////////////
;---------------------------------------------------------------------
    endif else begin
        ;Average value of t * Kij * vi (the dot product in IDL is the outer product).
        Kv_1 = [ mean(t * (Kij[*,0,0]*_v[*,0] + Kij[*,1,0]*_v[*,1] + Kij[*,2,0]*_v[*,2]) ), $
                 mean(t * (Kij[*,0,1]*_v[*,0] + Kij[*,1,1]*_v[*,1] + Kij[*,2,1]*_v[*,2]) ), $
                 mean(t * (Kij[*,0,2]*_v[*,0] + Kij[*,1,2]*_v[*,1] + Kij[*,2,2]*_v[*,2]) )]
        
        ;Average value of t*Kij and t^2 * Kij
        _t = rebin(double(t), npts, 3, 3)
        K_1 = MrMean(_t * Kij, DIMENSION=1)
        K_2 = MrMean(_t^2 * Kij, DIMENSION=1)
        _t  = 0B
        _v  = 0B
        Kij = 0B

        ;Compute the acceleration
        K_0_inv = invert(K_0)
        K_1_inv = invert(K_1)
        aHT = invert(K_0_inv # K_1 - K_1_inv # K_2) # (K_0_inv # Kv_0 - K_1_inv # Kv_1)
        
        ;Compute the initial velocity
        vHT = K_0_inv # Kv_0 - K_0_inv # (K_1 # aHT)
    endelse
	
	return, vHT
end


;
;EXAMPLE PROGRAM
;
;   This example program uses data from the Appendix at the end of Chapter 8 in ISSI 
;   "Analysis Methods for Multi-Spacecraft Data". Data points are from AMPTE/IRM, 
;   from 05:18:20–05:19:26 UT on October 19, 1984 and for use in benchmark tests
;   of MVA and other programs
;
;   The procedure that follows will duplicates the results from the text.
;

;---------------------------------------------------------------------
;Copy the Sample Data ////////////////////////////////////////////////
;---------------------------------------------------------------------
;time (s) seconds from 15:18:20.49
t = [0.00,4.38,8.75,13.12,17.49,21.87,26.24,30.61,34.98,39.36,43.73,48.10,52.47,56.85,61.22,65.59]

;density (#/cm^3)
n = [12.18,9.22,9.92,18.08,20.39,15.00,20.19,23.53,24.31,25.91,26.17,24.49,22.20,22.86,17.56,17.86]

;B(x,y,z) (nT)
Bx = transpose([-13.6,-14.8,-13.4,-14,-7.1,-0.9,-10,-6.1,1.2,-3.4,-0.9,-1,11,19.1,24.9,29.2])
By = transpose([-24.7,-24.9,-17.2,-25,-4.5,-5,-0.4,-4.8,1.6,-3.9,1.2,-1.5,13.2,34.4,50.1,47.1])
Bz = transpose([54.6,58.7,62.4,43.8,33.5,44.4,44.6,21.1,21,4.1,5,12.3,29.7,20.1,1.9,-10.6])

;Velocity (km/s)
Vx = transpose([-111,-102,-111,-135,-128,-82,-139,-143,-132,-117,-112,-98,-90,-83,-82,-93])
Vy = transpose([-211,-213,-196,-229,-252,-237,-228,-241,-226,-217,-210,-212,-186,-168,-129,-123])
Vz = transpose([57, 41, 34, 54, 54, 51, 77, 57, 80, 79, 93, 92, 104, 121, 88, 53])

;combine the components of the magnetic field and of the velocity
B = double([Bx, By, Bz])
v = double([Vx, Vy, Vz])

;---------------------------------------------------------------------
; Compute the Convective Electric Field //////////////////////////////
;---------------------------------------------------------------------
;Compute the convective electric field: E = -v x B
E = -[v[1,*]*B[2,*] - v[2,*]*B[1,*], $      ;Ex
      v[2,*]*B[0,*] - v[0,*]*B[2,*], $      ;Ey
      v[0,*]*B[1,*] - v[1,*]*B[0,*]]        ;Ez
E = E * 1.0e-3                              ;convert to mV/m

;---------------------------------------------------------------------
; Compute the deHoffmann-Teller Velocity /////////////////////////////
;---------------------------------------------------------------------
vHT     = MrHTVelocity(v, B)
vHT_acc = MrHTVelocity(v, B, t, AHT=aHT)

;---------------------------------------------------------------------
; Print the Results //////////////////////////////////////////////////
;---------------------------------------------------------------------
;deHoffmann-Teller Velocity
print, '-----------------------------------------------------'
print, 'deHoffmann-Teller Velocity'
print, FORMAT='(%"    vHT = [%8.3f, %8.3f, %8.3f] km/s")', vHT
print, 'Compare with the result on page 227, section 9.3.1.'
print, FORMAT='(%"    vHT = [%8.3f, %8.3f, %8.3f] km/s")', [-123, -223, -76]
print, ''

print, '-----------------------------------------------------'
print, 'Accelerated deHoffmann-Teller Velocity'
print, FORMAT='(%"    vHT = [%8.3f, %8.3f, %8.3f] km/s")', vHT_acc
print, FORMAT='(%"    aHT = [%8.3f, %8.3f, %8.3f] km/s^2")', aHT
print, 'Compare with the result on page 241, section 9.5.'
print, FORMAT='(%"    vHT = [%8.3f, %8.3f, %8.3f] km/s")', [-102, -204, -21]
print, FORMAT='(%"    aHT = [%8.3f, %8.3f, %8.3f] km/s")', [-0.62, -0.54, 0.88]
print, ''

;---------------------------------------------------------------------
; Plot the Results ///////////////////////////////////////////////////
;---------------------------------------------------------------------
;Magnetic Field
gBx = Plot(t, B[0,*], LAYOUT=[2,3,1], TITLE='AMPTE/IRM 10-Oct-1984', $
           COLOR='Blue', XSTYLE=1, YTITLE='B (nT)')
gBy = Plot(t, B[1,*], OVERPLOT=gBx, COLOR='Green')
gBz = Plot(t, B[2,*], OVERPLOT=gBx, COLOR='Red')

;Plasma Velocity
gVx = Plot(t, V[0,*], LAYOUT=[2,3,3], /CURRENT, TITLE='Plasma Density', $
           COLOR='Blue', XSTYLE=1, YTITLE='v (km/s)')
gVy = Plot(t, V[1,*], OVERPLOT=gVx, COLOR='Green')
gVz = Plot(t, V[2,*], OVERPLOT=gVx, COLOR='Red')

;Density
gN = Plot(t, n, LAYOUT=[2,3,5], /CURRENT, TITLE='Plasma Density', $
          XTITLE='Seconds Since 05:18:20.49 UT', $
          COLOR='Blue', XSTYLE=1, YTITLE='n ($cm^{-3}$)')


;Magnetic Field in MVA Frame (pg. 190 with x3 <-> x1, x2 -> -x2 to align with GSE)
mva_frame = [[ 0.8671, -0.4978,  0.0187], $     ;N
             [ 0.2866,  0.5326,  0.7597], $     ;M
             [-0.4016, -0.6845,  0.6055]]       ;L
Bmva = matrix_multiply(mva_frame, B, /ATRANSPOSE)
gBx = Plot(t, Bmva[0,*], LAYOUT=[2,3,2], /CURRENT, TITLE='AMPTE/IRM 10-Oct-1984', $
           COLOR='Blue', XSTYLE=1, YTITLE='B (nT)', NAME='$B_{N}$')
gBy = Plot(t, Bmva[1,*], OVERPLOT=gBx, COLOR='Green', NAME='$B_{M}$')
gBz = Plot(t, Bmva[2,*], OVERPLOT=gBx, COLOR='Red', NAME='$B_{L}$')

;Plasma Velocity in HT Frame
gVHTx = Plot(t, V[0,*]-vHT[0], LAYOUT=[2,3,4], /CURRENT, TITLE='Plasma Velocity in HT Frame', $
           COLOR='Blue', XSTYLE=1, YTITLE='$v-v_{HT}$ (km/s)')
gVHTy = Plot(t, V[1,*]-vHT[1], OVERPLOT=gVHTx, COLOR='Green')
gVHTz = Plot(t, V[2,*]-vHT[2], OVERPLOT=gVHTx, COLOR='Red')

;Plasma Velocity in accelerated HT Frame
gAHTx = Plot(t, V[0,*]-vHT[0]-aHT[0]*t, LAYOUT=[2,3,6], /CURRENT, TITLE='Accelerated HT Frame', $
           COLOR='Blue', XSTYLE=1, YTITLE='$v-(v_{HT}+a_{HT}t)$ (km/s)')
gAHTy = Plot(t, V[1,*]-vHT[1]-aHT[1]*t, OVERPLOT=gAHTx, COLOR='Green')
gAHTz = Plot(t, V[2,*]-vHT[2]-aHT[2]*t, OVERPLOT=gAHTx, COLOR='Red')

end