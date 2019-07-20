function wwevt = detectprofiles(pressure, timestamp, conductivity, pressureThreshold, conductivityThreshold)

%DETECTPROFILES - Implement the logger profile detection algorithm.
%
% Syntax:  [wwevt] = DETECTPROFILES(pressure, timestamp, conductivity, profileThreshold, conductivityThreshold)
% 
% Implements the algorithm used by the logger to find upcast and downcast
% events using the pressure reversals during the time series. The algorithm
% uses conductivity (if available) to detect when the logger is out of the
% water. The profiles do not include the out of water times. 
%
% Inputs:
%    pressure - Time series
%
%    timestamp - Time associated with the pressure time series
%
%    conductivity - Time series. Optional, if no conductivity data use []
%   
%    pressureThreshold - Pressure difference required to detect a
%               profile. Standard is 3dbar, or 0.05*(max(pressure)-min(pressure).
%
%    conductivityThreshold - Conductivity value that indicates the
%               sensor is out of water. Typically 0.05 mS/cm is very good. If the
%               water is fresh it may be better to use a lower value.
%
% Outputs:
%    wwevt - A matrix containing the timestamp in the first column and an
%            event index describing the start of a event (1=downcast,
%            2=upcast, 3=outofwater).
%
% See also: RSKfindprofiles.
%
% Author: RBR Ltd. Ottawa ON, Canada
% email: support@rbr-global.com
% Website: www.rbr-global.com
% Last revision: 2017-11-14

%% Set up
detectcaststate = 0; % 0 unknown, 1 down, 2 up
hasC = ~isempty(conductivity);



%% The Profile Detection
k = 1;
klast = k;
n = 1;
maxpressure = pressure(1);
minpressure = pressure(1);
wwevt = zeros(1,2);
while(k < length(timestamp))
    %
    % profile detection part
    evt = 0; % 0 nothing new 1 we are descending 2 we are ascending
    if hasC && conductivity(k) < conductivityThreshold
        evt = 3;
        minpressure = pressure(k);
    else
        
        switch  detectcaststate

            case 0   % unknown
                if (pressure(k) > maxpressure)
                    maxpressure = pressure(k);
                    if (maxpressure - minpressure > pressureThreshold)
                        detectcaststate = 1;
                        evt = 1;
                    end
                end
                if (pressure(k) < minpressure)
                    minpressure = pressure(k);
                    if (maxpressure - minpressure > pressureThreshold)
                        detectcaststate = 2;
                        evt = 2;
                    end
                end

            case 1   % down
                if (pressure(k) > maxpressure)
                    maxpressure = pressure(k);
                end
                if (pressure(k) < minpressure)
                    minpressure = pressure(k);
                end
                if (maxpressure - pressure(k) > max(pressureThreshold, 0.05*(maxpressure - minpressure))) % we are going up, set by profile detection algorithm
                    detectcaststate = 2;
                    evt = 2;
                    minpressure = pressure(k);
                else
                    detectcaststate = 1;  
                end


            case 2   % up
                if (pressure(k) > maxpressure)
                    maxpressure = pressure(k);
                end
                if (pressure(k) < minpressure)
                    minpressure = pressure(k);
                end
                if (pressure(k) - minpressure > max(pressureThreshold, 0.05*(maxpressure - minpressure))) % we are going down, set by profile detection algorithm
                    detectcaststate = 1;
                    evt= 1;
                    maxpressure = pressure(k);
                else
                    detectcaststate = 2;
                end

        end
    end

    
    
    if evt == 1
        % downcast detected
        profiletime = timestamp(klast:k);
        idx = find(pressure(klast:k) == minpressure);
        wwevt(n,:) = [profiletime(idx(end)) evt];
        n = n+1;
        klast = k;

    elseif evt ==2
        % upcast detected
        profiletime = timestamp(klast:k);
        idx = find(pressure(klast:k) == maxpressure);
        wwevt(n,:) = [profiletime(idx(end)) evt];
        n = n+1;
        klast = k;
        
    elseif evt == 3
        % If logger is out of water mark timestamp a out of water
        if n==1
            wwevt(n,:) = [timestamp(k) evt];
            n = n+1;
            klast = k;
        elseif wwevt(n-1,2) ~= 3
            wwevt(n,:) = [timestamp(k) evt];
            n = n+1;
            klast = k;
        end
    end
    k= k+1;
end

end