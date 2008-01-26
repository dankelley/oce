tidem.vuf <- function(t, j, lat=NULL)
{
    data("tidesetup")
    tideconst   <- get("tideconst",   pos=globalenv())
    tidesat     <- get("tidesat",     pos=globalenv())
    tidedoodson <- get("tidedoodson", pos=globalenv())
    ##    tidesemi    <- get("tidesemi",    pos=globalenv())
    a <- tidem.astron(t)
print(a)
    v <- tidedoodson[j,] %*% a$astro + tideconst$semi[j]
print(tideconst$semi[j])
    v <- v - trunc(v)
    if (!is.null(lat)) {
        if (abs(lat) < 5) lat <- sign(lat) * 5
        slat <- sin(pi * lat / 180)
        j <- which(tidesat$ilatfac == 1)
        rr    <- tidesat$amprat
        rr[j] <- rr[j] * 0.36309 * (1.0 - 5.0 * slat * slat) / slat
        j     <- which(tidesat$ilatfac == 2)
        rr[j] <- rr[j] * 2.59808 * slat

        uu <- tidesat$deldood %*% a$astro[4:6] + tidesat$phcorr
        uu <- uu - trunc(uu)

        cat("uu=");print(uu)

        nsat <- length(tidesat$iconst)
        nfreq <- length(tideconst$numsat)

        ##  fsum=1+sum(sparse([1:nsat],sat.iconst,rr.*exp(i*2*pi*uu),nsat,nfreq)).';

        ##>  fsum = 1 + sum(sparse([1:nsat], tidesat$iconst, rr*exp(i*2*pi*uu), nsat, nfreq)).'

        ##>> help sparse
        ##         SPARSE Create sparse matrix.
        ##            S = SPARSE(X) converts a sparse or full matrix to sparse form by
        ##            squeezing out any zero elements.
        ##
        ##            S = SPARSE(i,j,s,m,n,nzmax) uses the rows of [i,j,s] to generate an
        ##            m-by-n sparse matrix with space allocated for nzmax nonzeros.  The
        ##            two integer index vectors, i and j, and the real or complex entries
        ##            vector, s, all have the same length, nnz, which is the number of
        ##            nonzeros in the resulting sparse matrix S .  Any elements of s
        ##            which have duplicate values of i and j are added together.
        ##
        ##            There are several simplifications of this six argument call.
        ##
        ##            S = SPARSE(i,j,s,m,n) uses nzmax = length(s).
        ##
        ##            S = SPARSE(i,j,s) uses m = max(i) and n = max(j).
        ##
        ##            S = SPARSE(m,n) abbreviates SPARSE([],[],[],m,n,0).  This
        ##            generates the ultimate sparse matrix, an m-by-n all zero matrix.
        ##
        ##            The argument s and one of the arguments i or j may be scalars,
        ##            in which case they are expanded so that the first three arguments
        ##            all have the same length.
        ##
        ##            For example, this dissects and then reassembles a sparse matrix:
        ##
        ##                               [i,j,s] = find(S);
        ##                       [m,n] = size(S);
        ##                       S = sparse(i,j,s,m,n);
        ##
        ##            So does this, if the last row and column have nonzero entries:
        ##
        ##                               [i,j,s] = find(S);
        ##                       S = sparse(i,j,s);
        ##
        ##            All of MATLAB's built-in arithmetic, logical and indexing operations
        ##    can be applied to sparse matrices, or to mixtures of sparse and
        ##    full matrices.  Operations on sparse matrices return sparse matrices

        ##    and operations on full matrices return full matrices.  In most cases,
        ##    operations on mixtures of sparse and full matrices return full
        ##    matrices.  The exceptions include situations where the result of
        ##    a mixed operation is structurally sparse, eg.  A .* S is at least
        ##    as sparse as S .  Some operations, such as S >= 0, generate
        ##    "Big Sparse", or "BS", matrices -- matrices with sparse storage
        ##    organization but few zero elements.
        ##
        ##    See also ISSPARSE, SPALLOC, SPONES, SPEYE, SPCONVERT, FULL, FIND, SPARFUN.

        ##        ## HERE HERE HERE

        warning("not doing anything for u and f yet!")
        u <- rep(NA, length(v))
        f <- rep(NA, length(v))
    }
    else {
        u <- rep(0, length(v))
        f <- rep(1, length(v))
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
