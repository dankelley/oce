tidem.vuf <- function(t, j, lat=NULL)
{
    debug <- 0
    data("tidedata")
    tidedata   <- get("tidedata",   pos=globalenv())
    a <- tidem.astron(t)

    if (debug > 0) print(a)

    doodson <- cbind(tidedata$const$d1,
                     tidedata$const$d2,
                     tidedata$const$d3,
                     tidedata$const$d4,
                     tidedata$const$d5,
                     tidedata$const$d6)

    ##v=rem( const.doodson*astro+const.semi, 1);
    if (debug > 0) {
        cat("doodson[1,]=",doodson[1,],"\n")
        cat("doodson[2,]=",doodson[2,],"\n")
        cat("doodson[3,]=",doodson[3,],"\n")
    }

    v <- doodson %*% a$astro + tidedata$const$semi
    if (debug > 0) cat("tidedata$const$semi[",j,"]=",tidedata$const$semi[j],"\n")
    v <- v - trunc(v)
    if (debug > 0) cat("v[1:3]=",v[1:3],"\n")
    if (!is.null(lat)) {
        if (abs(lat) < 5) lat <- sign(lat) * 5
        slat <- sin(pi * lat / 180)
        k <- which(tidedata$sat$ilatfac == 1)
        rr    <- tidedata$sat$amprat
        rr[k] <- rr[k] * 0.36309 * (1.0 - 5.0 * slat * slat) / slat
        k     <- which(tidedata$sat$ilatfac == 2)
        rr[k] <- rr[k] * 2.59808 * slat

        uu <- tidedata$sat$deldood %*% a$astro[4:6] + tidedata$sat$phcorr
        uu <- uu - trunc(uu)

        if (debug > 1) {cat("uu[1:3]=");print(uu[1:3])}

        nsat <- length(tidedata$sat$iconst)
        nfreq <- length(tidedata$const$numsat)
                                        # loop, rather than make a big matrix
        if (debug > 2) {
            cat("tidedata$sat$iconst=", tidedata$sat$iconst, "\n")
            cat("length(sat$iconst)=", length(tidedata$sat$iconst),"\n")
        }
        fsum.vec <- vector("numeric", nsat)
        u.vec <- vector("numeric", nsat)
        for (isat in 1:nsat) {
            if (debug > 3) cat("isat=",isat,"\n")
            use <- tidedata$sat$iconst == isat
            fsum.vec[isat] <- 1 + sum(rr[use] * exp(1i * 2 * pi * uu[use]))
            u.vec[isat] <- Arg(fsum.vec[isat]) / 2 / pi
            if (isat==8 && debug > 0) {
                cat("TEST at isat=8:\n")
                cat("fsum.vec[",isat,"]=",fsum.vec[isat]," (EXPECT  1.18531604917590 - 0.08028013402313i)\n")
                cat("u.vec[   ",isat,"]=",u.vec[isat],"       (EXPECT -0.01076294959868)\n")
            }
        }
        if (debug > 0) {
            cat("uvec[",j,"]=", u.vec[j], "\n")
            cat("fsum.vec[",j,"]=", fsum.vec[j],"\n")
        }

        f <- abs(fsum.vec)
        u <- Arg(fsum.vec)/2/pi
        if (debug>3) cat("f=",f,"\n") # correct
        if (debug>3) cat("u=",u,"\n") # correct

        for (k in which(!is.na(tidedata$const$ishallow))) {
            ik <- tidedata$const$ishallow[k] + 0:(tidedata$const$nshallow[k] - 1)
            f[k] <- prod(f[tidedata$shallow$iname[ik]]^abs(tidedata$shallow$coef[ik]))
            u[k] <- sum(u[tidedata$shallow$iname[ik]]*tidedata$shallow$coef[ik])
            v[k] <- sum(v[tidedata$shallow$iname[ik]]*tidedata$shallow$coef[ik])
            if (debug>0 && k < 28) cat("k=",k,"f[k]=",f[k]," u[k]=",u[k],"v[k]=",v[k],"\n")
        }
        u <- u[j]
        v <- v[j]
        f <- f[j]
    }
    else {
        v <- v[j]
        u <- rep(0, length(j))
        f <- rep(1, length(j))
    }
    list(v=v, u=u, f=f)
}

#function [v,u,f]=t_vuf(ctime,ju,lat);
#% T_VUF Computes nodal modulation corrections.
#% [V,U,F]=T_VUF(DATE,JU,LAT) returns the astronomical phase V, the
#% nodal phase modulation U, and the nodal amplitude correction F at
#% a decimal date DATE for the components specified by index JU (into
#% the CONST structure returned by T_GETCONSTS) at a latitude LAT.
#%
#% If LAT is not specified, then the Greenwich phase V is computed with
#% U=0 and F=1.
#%
#% Note that V and U are in 'cycles', not degrees or radians (i.e.,
#% multiply by 360 to get degrees).
#%
#% If LAT is set to NaN, then the nodal corrections are computed for all
#% satellites that do *not* have a "latitude-dependent" correction
#% factor. This is for compatibility with the ways things are done in
#% the xtide package. (The latitude-dependent corrections were zeroed
#% out there partly because it was convenient, but this was rationalized
#% by saying that since the forcing of tides can occur at latitudes
#% other than where they are observed, the idea that observations have
#% the equilibrium latitude-dependence is possibly bogus anyway).
#
#% R. Pawlowicz 11/8/99
#%               1/5/00 - Changed to allow for no LAT setting.
#%              11/8/00 - Added the LAT=NaN option.
#% Version 1.0
#
#% Get all the info about constituents.
#
#[const,sat,shallow]=t_getconsts(ctime);
#
#% Calculate astronomical arguments at mid-point of data time series.
#[astro,ader]=t_astron(ctime);
#
#
#% Phase relative to Greenwich (in units of cycles, presumeably).
#% (This only returns values when we have doodson#s, i.e., not for the
#% shallow water components, but these will be computed later.)
#v=rem( const.doodson*astro+const.semi, 1);
#
#if nargin==3, % If we have a latitude, get nodal corrections.
#
#  % Apparently the second-order terms in the tidal potential go to zero
#  % at the equator, but the third-order terms do not. Hence when trying
#  % to infer the third-order terms from the second-order terms, the
#  % nodal correction factors blow up. In order to prevent this, it is
#  % assumed that the equatorial forcing is due to second-order forcing
#  % OFF the equator, from about the 5 degree location. Latitudes are
#  % hence (somewhat arbitrarily) forced to be no closer than 5 deg to
#  % the equator.
#
#  if finite(lat) & (abs(lat)<5); lat=sign(lat).*5; end
#
#  slat=sin(pi.*lat./180);
#  % Satellite amplitude ratio adjustment for latitude.
#
#  rr=sat.amprat;           % no amplitude correction
#
#  if finite(lat),
#    j=find(sat.ilatfac==1); % latitude correction for diurnal constituents
#    rr(j)=rr(j).*0.36309.*(1.0-5.0.*slat.*slat)./slat;
#
#    j=find(sat.ilatfac==2); % latitude correction for semi-diurnal constituents
#    rr(j)=rr(j).*2.59808.*slat;
#  else
#    rr(sat.ilatfac>0)=0;
#  end;
#
#  % Calculate nodal amplitude and phase corrections.
#
#  uu=rem( sat.deldood*astro(4:6)+sat.phcorr, 1);
#
#  %%uu=uudbl-round(uudbl);  <_ I think this was wrong. The original
#  %                         FORTRAN code is:  IUU=UUDBL
#  %                                           UU=UUDBL-IUU
#  %                         which is truncation.
#
#
#  % Sum up all of the satellite factors for all satellites.
#
#  nsat=length(sat.iconst);
#  nfreq=length(const.isat);
#
#  fsum=1+sum(sparse([1:nsat],sat.iconst,rr.*exp(i*2*pi*uu),nsat,nfreq)).';
#
#  f=abs(fsum);
#  u=angle(fsum)./(2.*pi);
#
#  % Compute amplitude and phase corrections for shallow water constituents.
#
#  for k=find(finite(const.ishallow))',
#    ik=const.ishallow(k)+[0:const.nshallow(k)-1];
#    f(k)=prod(f(shallow.iname(ik)).^abs(shallow.coef(ik)));
#    u(k)=sum( u(shallow.iname(ik)).*shallow.coef(ik) );
#    v(k)=sum( v(shallow.iname(ik)).*shallow.coef(ik) );
#  end;
#
#  f=f(ju);
#  u=u(ju);
#  v=v(ju);
#
#else % Astronomical arguments only.
#
#  % Compute for shallow water constituents.
#  for k=find(finite(const.ishallow))',
#    ik=const.ishallow(k)+[0:const.nshallow(k)-1];
#    v(k)=sum( v(shallow.iname(ik)).*shallow.coef(ik) );
#  end;
#  v=v(ju);
#  f=ones(size(v));
#  u=zeros(size(v));
#end;

