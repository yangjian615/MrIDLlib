pro test_pa_distribution, it
    
    catch, theError
    if theError ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif

    hope_file = '/Users/argall/Documents/Work/Data/RBSP/HOPE/rbspa_rel02_ect-hope-PA-L3_20130117_v4.0.0.cdf'
    hope_cdf = cdf_read(hope_file)
    H_data = hope_cdf -> read('FPDU', t_epoch, pa, energy)
    pa_minus = [4.5, replicate(9.0, 9), 4.5]
    pa_plus = [4.5, replicate(9.0, 9), 4.5]
    energy_minus = hope_cdf -> read('ENERGY_Ion_DELTA')
    energy_plus = energy_minus
    obj_destroy, hope_cdf

    nPA = n_elements(pa)
    nEnergy = n_elements(energy[*,it])
    nPts = 10

    image = replace_fillval(H_data[*,*,it], -1e31)
    image = replace_fillval(H_data[*,*,it], 0)
    image = bytscl(MrLog(image), /NAN)
;    energy = MrLog(energy)
;    energy_minus = MrLog(energy_minus)
;    energy_plus = MrLog(energy_plus)
    pa /= !radeg
    pa_plus /= !radeg
    pa_minus /= !radeg

    ;Create a polar plot with an outer ring at the highest energy level
    device, decomposed=0
    cgPlot, replicate(max(energy), nPA) + max(energy_plus), pa, /POLAR, ASPECT=1.0, /XLOG, /YLOG
    cgLoadCT, 1

    ;Draw each pixel
    for i = 0, nEnergy-1 do begin
        for j = 0, nPA-1 do begin
        
            pt = [energy[i,it], pa[j]]

            x0 = pt + [-energy_minus[i,it],  pa_plus[j]]
            x1 = pt + [-energy_minus[i,it], -pa_minus[j]]
            x2 = pt + [ energy_plus[i,it],  -pa_minus[j]]
            x3 = pt + [ energy_plus[i,it],   pa_plus[j]]

            E_x0_to_x1 = replicate(x0[0], nPts)
            PA_x0_to_x1 = linspace(x0[1], x1[1], nPts)
        
            E_x1_to_x2 = linspace(x1[0], x2[0], nPts)
            PA_x1_to_x2 = replicate(x1[1], nPts)
        
            E_x2_to_x3 = replicate(x2[0], nPts)
            PA_x2_to_x3 = linspace(x2[1], x3[1], nPts)
        
            E_x3_to_x1 = linspace(x3[0], x3[0], nPts)
            PA_x3_to_x1 = replicate(x3[1], nPts)
            
            ;x = r * cos(theta)
            ;y = r * sin(theta)
            r = [ E_x0_to_x1,  E_x1_to_x2,  E_x2_to_x3,  E_x3_to_x1]
            theta = [PA_x0_to_x1, PA_x1_to_x2, PA_x2_to_x3, PA_x3_to_x1]

            x = r * cos(theta)
            y = r * sin(theta)

            polyfill, x, y, COLOR=image[i,j], /DATA
        endfor
    endfor
end