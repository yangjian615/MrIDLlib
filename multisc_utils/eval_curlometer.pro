pro curlometer_getData, ref_sc, date, tstart, tend, $
FIELDS=fields, $
TIMES=times, $
POSITIONS=positions, $
PTIMES = ptimes
    compile_opt idl2
    
    ;Number of spacecraft
    N = 4
    
    ;create a N element pointer array for position and field data and their corresponding
    ;time stamps. N is the number of spacecraft.
    positions = ptrarr(N)
    fields = ptrarr(N)
    times = ptrarr(N)
    ptimes = ptrarr(N)
    
    ;step through the spacecraft, getting their positions, magnetic fields, and time 
    ;stamps.
    for sc = 1, N do begin
        positions[sc-1] = ptr_new(curlometer_getPositions(sc, date, tstart, tend, PTIME=pt))
        fields[sc-1] = ptr_new(curlometer_getField(sc, date, tstart, tend, TIME=t))
        times[sc-1] = ptr_new(t)
        sc = fix(sc)
        ptimes[sc-1] = ptr_new(pt)
    endfor
    
end


function curlometer_getPositions, sc, date, tstart, tend, $
DIRECTORY = directory, $
PTIME = ptime
    compile_opt idl2
    ;create a new fgm object and extract time and position data, then destroy the object
    ofgm = obj_new('FGM', sc, date, tstart, tend, DIRECTORY=directory)
    varnames = ofgm -> comp_map(['POS_X', 'T'])
    ofgm -> loadRecRange, varnames[1]
    ofgm -> getProperty, REC_START=rec_start, REC_END=rec_end
    ofgm -> read, varnames, REC_START=rec_start, REC_END=rec_end
    ofgm -> getProperty, DATA=data
    ptime = data[varnames[1]].data
    ptime = ofgm -> convert_epoch_to_ssm(ptime)
    positions = data[varnames[0]].data

    ;Now remove any fill values from the field
    ;Start by getting the multi-dimensional indices of the non-fill value data points
    !Null = ofgm -> inds_nofillval(varnames[0], COMPLEMENT=bad_inds, NCOMPLEMENT=count)
    if count ne 0 then field = curlometer_removeGaps(bad_inds, time, field, MASKED_TIME=time)

    obj_destroy, ofgm
    
    return, positions
end
    

function curlometer_getField, sc, date, tstart, tend, $
DIRECTORY = directory, $
TIME = time
    compile_opt idl2

    ;create a new fgm object and extract time and position data, then destroy the object
    ofgm = obj_new('FGM', sc, date, tstart, tend, DIRECTORY=directory)
    varnames = ofgm -> comp_map(['BX', 'T'])
    ofgm -> loadRecRange, varnames[1]
    ofgm -> getProperty, REC_START=rec_start, REC_END=rec_end
    ofgm -> read, varnames, REC_START=rec_start, REC_END=rec_end
    ofgm -> getProperty, DATA=data
    time = data[varnames[1]].data
    time = ofgm -> convert_epoch_to_ssm(time)
    field = data[varnames[0]].data
    
    ;Now remove any fill values from the field
    ;Start by getting the multi-dimensional indices of the non-fill value data points
    !Null = ofgm -> inds_nofillval(varnames[0], COMPLEMENT=bad_inds, NCOMPLEMENT=count)
    if count ne 0 then field = curlometer_removeGaps(bad_inds, time, field, MASKED_TIME=time)

    obj_destroy, ofgm
    
    ;return the positions
    return, field
end


function curlometer_removeGaps, bad_inds, time, data, $
MASKED_TIME = masked_time, $
SINGLE_COMP = single_comp
    compile_opt idl2
    
    ;DATA is assumed to be a 3xN array where 3 represents the 3 components: x, y, and z;
    ;while N is the total number of points in each dimension and is equal to the number
    ;of points in TIME. If the i-th element of component x is marked as one of the
    ;BAD_INDS, then elements TIME[i] and DATA[*,i] will be removed.
    
    ;get the dimensions and number of components in DATA.
    dims = size(data, /dimensions)
    npts = dims[1]
    
    ;convert the 1D indices to multi-demensional indices. The result is 2D array since
    ;DATA is 3xN (i.e., it has 2 dimensions)
    imultid = array_indices(dims, bad_inds, /DIMENSIONS)
    
    ;A value could be bad at the same time stamp in more than one component. Get only the
    ;indices that are unique across all components.
    unique = uniq(imultid[1,*], sort(imultid[1,*]))
    
    ;Find the elements that are not members of the bad_inds set.
    !null = ismember(lindgen(npts), imultid[1,unique], NONMEMBERS=nonmembers)    
    masked_time = time[nonmembers]
    return, data[*, nonmembers]
end


function curlometer_interpolate, ref_sc, times, data
    compile_opt idl2

    ;get the number of spacecraft and a list of the ones that are not the reference sc.
    nsc = n_elements(times)
    spacecraft = intarr(nsc) + 1
    nonref_sc = spacecraft[where(spacecraft ne ref_sc)]
    
    ;interpolate the data to the reference spacecraft time stamps.
    foreach sc, nonref_sc - 1 do begin
        ;only interpolate if the time stamps are different
        if ~array_equal(*times[sc], *times[ref_sc-1]) then begin
            data[sc] = ptr_new(interpol_vec(*data[sc], *times[sc], *times[ref_sc-1]))
            stop
        endif
    endforeach
    
    ;return the interpolated data
    return, data
    
end


function curlometer_syncTimes, ref_sc, times, $
NONREF = nonref
    compile_opt idl2

    ;
    ;Find where the NONREF_SC times fit into the REF_SC times. NONREF_SC times will be
    ;rounded down to the nearest matching REF_SC time. In trying to sync the times of the
    ;two spacecraft, if any point in REF_SC is greater than +/-dt from NONREF_SC, then
    ;there is a data gap and points have to be removed from one of the data sets. These
    ;gaps can be found using VALUE_LOCATE() to create a map between REF_SC and NONREF_SC
    ;   a) Repeated values within the map will indicate that REF_SC has data gaps not 
    ;       present in NONREF_SC.
    ;   b) A difference between the REF_SC[MAP] times and those with NONREF_SC having 
    ;       values greater than "dt" indicate a gap in NONREF_SC that is not present in 
    ;       REF_SC.
    ;These "extra" points in REF_SC mentioned in b) will be overlooked automatically in 
    ;the creation of the map. The repeated values in a) can be removed with the UNIQ()
    ;function.
    ;
    ;NOTE:
    ;   Since VALUE_LOCATE() rounds down, if NONREF_SC is delayed in its measurements ever
    ;   so slightly from the measurements of REF_SC, rounding down could create a systematic
    ;   gap of ~1*dt where rounding up would leave a gap of ~0*dt... perhaps this does
    ;   not matter because we will be interpolating right after this... so long as the data
    ;   gaps are gone.
    ;
    ;In the following scenario, the reference spacecraft has a data gap. Using 
    ;VALUE_LOCATE() to synch the times will map points 3, 4, 5 and 6 of the non-reference
    ;spacecraft (SC from here on out) to point 3 of the reference spacecraft (REF_SC).
    ;
    ; REF_SC -> 1.2.3.......7.8.9
    ;     SC -> 1.2.3.4.5.6.7.8.9
    ;
    ;If there is a data gap in SC, as below, then points 4, 5, and 6 of REF_SC will be
    ;ignored while by VALUE_LOCATE()
    ;
    ; REF_SC -> 1.2.3.4.5.6.7.8.9
    ;     SC -> 1.2.3.......7.8.9
    ;
    ;If the non-reference spacecraft is delayed with respect to the refrence SC, then
    ;rounding down is ok. Points 1-6 of SC will be mapped to points A-F of REF_SC in a
    ;1-to-1 manner.
    ;
    ; REF_SC -> A...B...C...D...E...F
    ;     SC -> .1...2...3...4...5...6
    ;
    ;If a data gap in REF_SC exists, the proper points in SC will be kept. Points 1, 2,
    ;4, 5, and 6 of SC will be mapped to A, B, D, E, F of REF_SC, respectively -- their
    ;closest neighbors in time.
    ;
    ; REF_SC -> A...B.......D...E...F
    ;     SC -> .1...2...X...4...5...6
    ;
    ;However, if the SC is ahead of REF_SC, points 2-6 of SC will be mapped to points A-E
    ;of REF_SC, even though they are closer to points B-F.
    ;
    ; REF_SC -> .A...B...C...D...E...F
    ;     SC -> 1...2...3...4...5...6
    ;
    ;then a data gap in REF_SC cause the wrong point in SC to be kept.
    ;
    ; REF_SC -> .A...B.......D...E...F
    ;     SC -> 1...2...3...X...5...6
    ;
    ;Therefore, both the mapped points and the points immediately after them need to
    ;be checked. A difference can be taken between SC time and these two sets of points. 
    ;Any difference less than 0.5*MIN_DT should be kept.
    ;
    ;That is, in the last example, points 1-6 of SC need to be checked against
    ;points A-E (as VALUE_LOCATE() does), as well as against points B-F. In this manner,
    ;point 3 of SC will be discarded instead of point 4.
    ;

    ;make an array with the non-reference spacecraft numbers
    nsc = n_elements(times)
    spacecraft = findgen(nsc) + 1
    nonref_sc = spacecraft[where(spacecraft ne ref_sc)]
    
    ;a pointer array to the synced indices of each spacecraft
    isync = ptrarr(nsc)
    
    ;Make an array of all of the reference spacecraft's time indices. These will be
    ;selected out if the other spacecraft do not have corresponding times.
    if ~keyword_set(NONREF) then member_list = lindgen(n_elements(*times[ref_sc-1]))
    
    ;for each of the non-reference spacecraft 
    foreach sc, nonref_sc-1 do begin
    
        ;get the sampling rate of each spacecraft
        dt_ref = curlometer_dt(*times[sc])
        dt_sc = curlometer_dt(*times[sc])
        dt_min = min([dt_ref, dt_sc], max=dt_max)
        
        ;The ratio of the number of samples
        dt_ratio = dt_max / dt_min
        
        ;create the map between REF_SC times and SC times
        ref_to_nonref_map = value_locate(*times[ref_sc-1], *times[sc])
                
        ;if any points in t_NONREF_SC preceed those of t_REF_SC, then set them equal to the
        ;first value of t_REF_SC
        preceed = where(ref_to_nonref_map eq -1, count)
        if count ne 0 then ref_to_nonref_map[preceed] = 0
        
        ;if REF_TO_NONREF_MAP are the points in t_REF_SC immidiately before those in t_SC
        ;then MAP_PLUS_ONE are those points immediately after t_Sc. Make sure they do not
        ;extend beyond the size of t_REF_SC.
        map_plus_one = ref_to_nonref_map + 1
        nref = n_elements(*times[ref_sc-1])
        exceed = where(map_plus_one ge nref, count)
        if count ne 0 then map_plus_one[exceed] = nref - 1
        
        ;take the difference between the previous (next) points of REF_SC and those of 
        ;NONREF_SC.
        diff_minus = abs((*times[ref_sc-1])[ref_to_nonref_map] - *times[sc])
        diff_plus  = abs((*times[ref_sc-1])[map_plus_one] - *times[sc])
        
        ;A tolerance needs to be set. If the sample rates are the same, then we want to look
        ;for differences that are +/- 0.5*dt from each other (tolerance = 0.5*dt). If the 
        ;sampling periods are different, then the tolerance needs to be adjusted.
        
        ;if the sampling periods are within 5% of being the same...
        if dt_ratio - 1 lt 0.05 then begin
            
            tolerance = 0.45*dt_min
            
            ; 1.......|.......2.......|.......3
            ; ol---->...<----tol---->...<----to
            
            ;Keep the points that are within the tolerance
            keep_minus = where(diff_minus lt tolerance, minus_count)
            keep_plus = where(diff_plus lt tolerance, plus_count)
            
            ;As a second step (first step is the ELSE condition), use t_REF_SC, which now
            ;only contains times that are common to all spacecraft, to weed out times from
            ;each SC that are not present in the others.
            if keyword_set(NONREF) then begin
                ;create the initial member list -- all the indices of the time array.
                ;Indices corresponding to times not also present in t_REF_SC will be
                ;thrown away.
                member_list = lindgen(n_elements(*times[sc]))
            
                ;collect all of the keeper points.
                if minus_count gt 0 AND plus_count gt 0 then nonref_keepers = [keep_minus, keep_plus] $
                else if minus_count gt 0 then nonref_keepers = keep_minus $
                else if plus_count  gt 0 then nonref_keepers = keep_plus $
                else message, 'No points within tolerance: +/- 0.45*dt'
            
                ;only keep the unique indices. These indices refer to the values of t_NONREF_SC
                ;that are close to t_REF_SC. They also refer to the elements of the MAP that trace
                ;from t_NONREF_SC back to t_REF_SC.
                
                ;indices may not be unique, e.g., because of PRECEED and EXCEED above.
                nonref_keepers = nonref_keepers[uniq(nonref_keepers, sort(nonref_keepers))]
                
                ;Get the indices of the MEMBER_LIST that also appear in the MAP. Update
                ;the MEMBER_LIST and store it as the
                !Null = ismember(nonref_keepers, member_list, INDICES=indices)
                isync[sc] = ptr_new(member_list[indices], /no_copy)
            
            ;As a first step, find the times in t_REF_SC that are common to all of the
            ;spacecraft. If a particular time element is present in
            endif else begin
            
                ;collect all of the keeper points.
                if minus_count gt 0 AND plus_count gt 0 then $
                    ref_keepers = [ref_to_nonref_map[keep_minus], map_plus_one[keep_plus]] $
                else if minus_count gt 0 then ref_keepers = ref_to_nonref_map[keep_minus] $
                else if plus_count  gt 0 then ref_keepers = map_plus_one[keep_plus] $
                else message, 'No points within tolerance: +/- 0.45*dt'
            
                ;only keep the unique indices. These indices refer to the values of t_NONREF_SC
                ;that are close to t_REF_SC. They also refer to the elements of the MAP that trace
                ;from t_NONREF_SC back to t_REF_SC.
                
                ;indices may not be unique because of PRECEED and EXCEED above.
                ref_keepers = ref_keepers[uniq(ref_keepers, sort(ref_keepers))]
                
                ;Get the indices of the MEMBER_LIST that also appear in the MAP. Update
                ;the MEMBER_LIST and store it as the
                !Null = ismember(ref_keepers, member_list, INDICES=indices)
                member_list = member_list[indices]
            endelse
            
        endif else begin
            message, 'The spacecraft have different sampling rates. This has not been taken ' + $
                     'into consideration yet.'
        endelse
    endforeach
        
    ;Now, MEMBER_LIST contains all of the times in t_REF_SC that are common to all
    ;N spacecraft. The next step is to use these times to select the corresponding
    ;times out of each non-reference spacecraft.
    if ~keyword_set(NONREF) then begin
    
        ;store the indices that sychronize REF_SC to all N-1 SC
        isync[ref_sc-1] = ptr_new(member_list)
        
        ;update t_REF_SC to contain only those times that are common to all SC
        times[ref_sc-1] = ptr_new((*times[ref_sc-1])[member_list])
        
        ;recursively call syncTimes, but this time use t_REF_SC to update each
        ;of the remaining spacecraft.
        isync[nonref_sc-1] = curlometer_syncTimes(ref_sc, times, /NONREF)
    endif else begin
    
        ;return the synchronized indices for all of the non-reference spacecraft
        return, isync[nonref_sc - 1]
    endelse
    
    ;finally, return the synchronized indices of all spacecraft
    return, isync

end


pro curlometer_syncFieldsPos, time, fields, ptime, positions
    compile_opt idl2
    
    ;figure out which array has the longest sampling period and interpolate the other
    ;to it. If they have the same sampling, then interpolate the positions to the fields.
    dt_fields = curlometer_dt(*time)
    dt_pos = curlometer_dt(*ptime)
    
    ;interpolate the positions to the fields
    if dt_fields ge dt_pos then begin
        ;make a pointer array of the times to be synced together
        toSync = [time, ptime]
        
        ;get the indices of the time values of PTIME that are close enough to the
        ;times in TIME that they can be interpolated. Reduce PTIME and POSITIONS to
        ;only those indices
        isync = curlometer_syncTimes(1, toSync)
        ptime = ptr_new((*ptime)[*isync[1]])
        foreach sc, spacecraft-1 do positions[sc] = ptr_new((*positions[sc])[*, isync[1]])
        
        ;interpolate the positions to be synchronous with the fields.
        positions = curlometer_interpolate(1, toSync, positions)
    
    ;interpolate the fields to the positions
    endif else begin
        ;make a pointer array of the times to be synced together
        toSync = [ptime, time]
        
        ;get the indices of the time values of TIME that are close enough to the
        ;times in PTIME that they can be interpolated. Reduce TIME and FIELDS to
        ;only those indices
        isync = curlometer_syncTimes(1, toSync)
        time = ptr_new((*time)[*isync[1]])
        foreach sc, spacecraft-1 do fields[sc] = ptr_new((*fields[sc])[*, isync[1]])
        
        ;interpolate the fields to be synchronous with the positions.
        positions = curlometer_interpolate(1, toSync, fields)
        
    endelse
    
    ;get rid of the pointer array. TIME and PTIME pointers remain valid.
    toSync = !Null    
end


function curlometer_dt, time
    compile_opt idl2
    
    npts = n_elements(time)    
    dt = (time[npts-1] - time[0]) / npts
    
    return, dt
end


function eval_curlometer, ref_sc, date, tstart, tend, $
FIELDS=fields, $
TIMES=times, $
POSITIONS=positions, $
PTIMES=ptimes
    compile_opt idl2
    
    ;get the data from the data files and remove any bad values that they have
    curlometer_getData, ref_sc, date, tstart, tend, $
                        TIMES=times, FIELDS=fields, POSITIONS=positions, PTIMES=ptimes
    
    ;find times common to all spacecraft to within a window of 0.5*dt, where dt is the
    ;sampling period.
    ifield_sync = curlometer_syncTimes(ref_sc, times)
    ipos_sync = curlometer_syncTimes(ref_sc, ptimes)

    ;take the times common to all spacecraft
    nsc = n_elements(times)
    spacecraft = indgen(nsc) + 1
    
    foreach sc, spacecraft-1 do begin
        times[sc] = ptr_new((*times[sc])[*ifield_sync[sc]])
        fields[sc] = ptr_new((*fields[sc])[*, *ifield_sync[sc]])
        ptimes[sc] = ptr_new((*ptimes[sc])[*ipos_sync[sc]])
        positions[sc] = ptr_new((*positions[sc])[*, *ipos_sync[sc]])
    endforeach
    
    ;interpolate the data to be exactly synchronous
    fields = curlometer_interpolate(ref_sc, times, fields)
    time = ptr_new(*times[ref_sc-1])
    ptr_free, times
    
    positions = curlometer_interpolate(ref_sc, ptimes, positions)
    ptime = ptr_new(*ptimes[ref_sc-1])
    ptr_free, ptimes

    ;if the times of the positions and those of the fields are different, then make them
    ;the same.
    if ~array_equal(*ptime, *time) then $
        curlometer_syncFieldsPos, time, fields, ptime, positions
    
    ;free PTIME as TIME can now be used
    ptr_free, ptime
    
    ;finally, evaluate the current via the curlometer
    J_curl = curlometer(positions, fields)
    
    ;Now calcluate the Current using the reciprocal vectors
    ;J = (Del x B) / mu_0
    mu_0 = 1.25663706e-6                ;m kg s-2 A-2
    Jrv = fltarr(size(*fields[0], /dimensions))
    Jrv_sum = fltarr(size(*fields[0], /dimensions))
    recVect = reciprocalVectors(positions)
    for i = 0, nsc - 1 do $
        ;leave as nA/m^2
        Jrv += cross_product(*recVect[i], *fields[i]) / (1e3 * mu_0)    ;nA -> microA    
    
    ;Plot the fields and the current from the curlometer method
    curlPlot = obj_new('mrplot', *time, *Fields[0], $
                       TITLE='Magnetic Field (GSE)', $
                       YTITLE='B (nT)', YRANGE=[min(*Fields[0], max=Fmax), Fmax], $
                       XTITLE='UT (HH:MM:SS)', XSTYLE=1, XTICKFORMAT='time_labels', $
                       LEGEND=['X', 'Y', 'Z'], COLOR=['blue', 'green', 'red'])
    
    curlPlot -> addPlot, *time, Jrv, $
                          TITLE='Current via Reciporcal Vectors', $
                          YTITLE='J (uA / m^2)', YRANGE=[min(Jrv, max=Jmax), Jmax], $
                          XTITLE='UT (HH:MM:SS)', XSTYLE=1, XTICKFORMAT='time_labels', $
                          LEGEND=['X', 'Y', 'Z'], COLOR=['blue', 'green', 'red']
    
    curlPlot -> addPlot, *time, J_curl, $
                         TITLE='Curlometer', $
                         YTITLE='J (uA / m^2)', YRANGE=[min(J_curl, max=Jmax), Jmax], $
                         XTITLE='UT (HH:MM:SS)', XSTYLE=1, XTICKFORMAT='time_labels', $
                         LEGEND=['X', 'Y', 'Z'], COLOR=['blue', 'green', 'red']
   
   ;free all of the pointers
   ptr_free, time, fields, positions
   return, curlPlot
end