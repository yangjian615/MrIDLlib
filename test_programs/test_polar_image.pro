pro test_polar_image, theTrial
    compile_opt idl2
    
    catch, the_error
    if the_error ne 0 then begin
        catch, /CANCEL
        if n_elements(thisDecomp) ne 0 then device, DECOMPOSED=thisDecomp
        if n_elements(r)          gt 0 then tvlct, r, g, b
        void = cgErrorMSG(/QUIET)
        return
    endif
    
    if n_elements(theTrial) eq 0 then theTrial = 7

    ;Create the grid spacing
    L    = linspace(  0,  9, 0.5, /INTERVAL)
    MLT  = linspace(  0, 24, 1.0, /INTERVAL)
    MLat = linspace(-45, 45, 1.0, /INTERVAL)

    ;Redefine MLT to be between 0 and 360 degrees
    MLT = MLT * (2*!pi / 24D)
    
    ;How many bins were created?
    L_nBins    = n_elements(L)
    MLT_nBins  = n_elements(MLT)
    MLat_nBins = n_elements(MLat)

    ;Create the 2D grid
    L = rebin(L, L_nBins, MLT_nBins)
    MLT = rebin(reform(MLT, 1, MLT_nBins), L_nBins, MLT_nBins)
    
    ;Set up the data coordinate system
    cgPlot, L[L_nBins-1, *], MLT[L_nBins-1,*], /POLAR, ASPECT=1D, /NODATA

    ;Create an image
    theImage = bytscl(dist(L_nBins, MLT_nBins))

    device, GET_DECOMPOSED=thisDecomp
    tvlct, r, g, b, /GET

    device, DECOMPOSED=0
    cgLoadCT, 13


    case theTrial of
        ;Lower-Left corner of the pixel location was given
        0: begin
            x = L * cos(MLT)
            y = L * sin(MLT)
            for i = 0, L_nBins - 2 do begin
                for j = 0, MLT_nBins - 2 do begin
                    xpoly = [x[i,j], x[i+1,j], x[i+1,j+1], x[i,j+1], x[i,j]]
                    ypoly = [y[i,j], y[i+1,j], y[i+1,j+1], y[i,j+1], y[i,j]]
                    polyfill, xpoly, ypoly, COLOR=theImage[i,j]
                endfor
            endfor
        endcase
        
        ;MrPixelPoints -- Lower-left corner of pixels was given in polar coordinates
        1: begin
            MrPixelPoints, theImage, L, MLT, xMin, yMin, xMax, yMax
            for i = 0, L_nBins - 1 do begin
                for j = 0, MLT_nBins - 1 do begin
                    x1 = xMin[i,j] * cos(yMin[i,j])
                    x2 = xMax[i,j] * cos(yMin[i,j])
                    x3 = xMax[i,j] * cos(yMax[i,j])
                    x4 = xMin[i,j] * cos(yMax[i,j])
                    
                    y1 = xMin[i,j]*sin(yMin[i,j])
                    y2 = xMax[i,j]*sin(yMin[i,j])
                    y3 = xMax[i,j]*sin(yMax[i,j])
                    y4 = xMin[i,j]*sin(yMax[i,j])
                
                    polyfill, [x1, x2, x3, x4, x1], [y1, y2, y3, y4, y1], COLOR=theImage[i,j]
                endfor
            endfor
        endcase
        
        ;MrPixelPoints -- center of pixels was given in polar coordinates
        2: begin
            MrPixelPoints, theImage, L, MLT, xMin, yMin, xMax, yMax, /CENTER
            for i = 0, L_nBins - 1 do begin
                for j = 0, MLT_nBins - 1 do begin
                    x1 = xMin[i,j] * cos(yMin[i,j])
                    x2 = xMax[i,j] * cos(yMin[i,j])
                    x3 = xMax[i,j] * cos(yMax[i,j])
                    x4 = xMin[i,j] * cos(yMax[i,j])
                    
                    y1 = xMin[i,j]*sin(yMin[i,j])
                    y2 = xMax[i,j]*sin(yMin[i,j])
                    y3 = xMax[i,j]*sin(yMax[i,j])
                    y4 = xMin[i,j]*sin(yMax[i,j])
                
                    polyfill, [x1, x2, x3, x4, x1], [y1, y2, y3, y4, y1], COLOR=theImage[i,j]
                endfor
            endfor
        endcase
        
        ;MraImage with the POLAR keyword set.
        3: begin
            position = MrLayout([1,1], 1, /SQUARE)
            mraImage, theImage, L, MLT, /POLAR, /AXES, CTINDEX=13, XRANGE=[-10,10], $
                                        YRANGE=[-10,10], POSITION=position
        endcase
        
        ;Polar plot on log scale
        4: begin
            L_dMinus   = 0.25
            L_dPlus    = 0.25
            MLT_dMinus = 0.5 * !dpi / 24D
            MLT_dPlus  = 0.5 * !dpi / 24D
            
            ;Calculate the pixel locations
            MrPixelDeltas, theImage, L, MLT, L_dMinus, MLT_dMinus, L_dPlus, MLT_dPlus, xMin, yMin, xMax, yMax
            position = MrLayout([1,1], 1, /SQUARE)
            mraImage, theImage, xMin, yMin, xMax, yMax, $
                      /POLAR, /AXES, CTINDEX=13, XRANGE=[-10,10], $
                      YRANGE=[-10,10], POSITION=position
        endcase
        
        ;Pixel Deltas with exponential radial direction
        5: begin
            ;An image with 36 energy channels and 11 pitch angle channels
            theImage  = dist(36, 11)
            
            ;Logarithmically spaced energy channels with +/-
            Energy    = logspace(2, 4.4, 36)
            E_DPlus   = [(Energy[1:*] - Energy[0:-2]) / 2, (Energy[-1] - Energy[-2]) / 2]
            E_DMinus  = [Energy[0]/2.0, (Energy[1:*] - Energy[0:-2]) / 2]
            
            ;Pitch angle sectors with +/-
            PA        = [4.5, linspace(18, 162, 9), 175.5] * !dtor
            PA_DPlus  = [4.5, replicate(9, 9), 4.5] * !dtor
            PA_DMinus = [4.5, replicate(9, 9), 4.5] * !dtor

            ;Display the image
            position = MrLayout([1,1], 1, /SQUARE)
            limit    = max(Energy + E_DPlus)
            mraImage, theImage, Energy, PA, E_DMinus, PA_DMinus, E_DPlus, PA_DPlus, $
                      /POLAR, /AXES, CTINDEX=13, XRANGE=[-limit, limit], $
                      YRANGE=[-limit, limit], POSITION=position
        endcase
        
        ;Pixel Deltas with logarithmically scaled radial direction
        6: begin
            ;An image with 36 energy channels and 11 pitch angle channels
            theImage  = dist(36, 11)
            
            ;Logarithmically spaced energy channels with +/-
            Energy    = logspace(2, 4.4, 36)
            E_DPlus   = [(Energy[1:*] - Energy[0:-2]) / 2, (Energy[-1] - Energy[-2]) / 2]
            E_DMinus  = [Energy[0]/2.0, (Energy[1:*] - Energy[0:-2]) / 2]
            
            ;Pitch angle sectors with +/-
            PA        = [4.5, linspace(18, 162, 9), 175.5] * !dtor
            PA_DPlus  = [4.5, replicate(9, 9), 4.5] * !dtor
            PA_DMinus = [4.5, replicate(9, 9), 4.5] * !dtor

            ;Display the image
            ;   - Cannot log-scale the radial direction and still get a circle.
            ;   - A negative log-scaled radius is not possible.
            ;
            ;Before log-scaling
            ;   - Must choose corners of the pixels
            r_min    = Energy - E_DMinus
            r_max    = [r_min[1:*], Energy[-1] + E_DPlus[-1]]
            t_min    = PA - PA_DMinus
            t_max    = [t_min[1:*], PA[-1] + PA_DPlus[-1]]

            position = MrLayout([1,1], 1, /SQUARE)
            limit    = max(MrLog(r_max))
            mraImage, theImage, MrLog(r_min), t_min, MrLog(r_max), t_max, $
                      /POLAR, /AXES, CTINDEX=13, XRANGE=[-limit, limit], $
                      YRANGE=[-limit, limit], POSITION=position
        endcase
        
        ;Pixel Deltas with logarithmically scaled radial direction using mraimage
        7: begin
            ;An image with 36 energy channels and 11 pitch angle channels
            theImage  = dist(36, 11)
            
            ;Logarithmically spaced energy channels with +/-
            Energy    = logspace(2, 4.4, 36)
            E_DPlus   = [(Energy[1:*] - Energy[0:-2]) / 2, (Energy[-1] - Energy[-2]) / 2]
            E_DMinus  = [Energy[0]/2.0, (Energy[1:*] - Energy[0:-2]) / 2]
            
            ;Pitch angle sectors with +/-
            PA        = [4.5, linspace(18, 162, 9), 175.5] * !dtor
            PA_DPlus  = [4.5, replicate(9, 9), 4.5] * !dtor
            PA_DMinus = [4.5, replicate(9, 9), 4.5] * !dtor

            ;Create the image
            position = MrLayout([1,1], 1, /SQUARE)
            limit    = max(MrLog(Energy + E_DPlus))
            mraImage, theImage, Energy, PA, E_DMinus, PA_DMinus, E_DPlus, PA_DPlus, $
                      /POLAR, /RLOG, /AXES, CTINDEX=13, XRANGE=[-limit, limit], $
                      YRANGE=[-limit, limit], POSITION=position
        endcase
    endcase
    
    device, DECOMPOSED=thisDecomp
    tvlct, r, g, b
    ;Draw concentric circles
;    cgPlot, L[L_nBins-1, *], MLT[L_nBins-1,*], /POLAR, ASPECT=1D, /NOERASE;, XRANGE=[-1,1], YRANGE=[-1,1]
;    for i = L_nBins - 2, 0, -1 do cgPlot, L[i,*], MLT[i,*], /POLAR, /NOERASE, /OVERPLOT

    ;Draw pinwheel spokes
;    for i = 0, MLT_nBins-1, 1 do cgPlot, [L[0,i], L[-1,i]], [MLT[0,i], MLT[-1,i]], /POLAR, /OVERPLOT
end
