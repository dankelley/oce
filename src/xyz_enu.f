      SUBROUTINE sfm_enu3(n, he, pi, ro,
     & st, fo, ma,
     & ea, no, up)
      DOUBLE PRECISION he(n), pi(n), ro(n)
      DOUBLE PRECISION st(n), fo(n), ma(n) 
      DOUBLE PRECISION ea(n), no(n), up(n)
      DOUBLE PRECISION CH(n), SH(n), CP(n), SP(n), CR(n), SR(n)
      DO i = 1, n
        h = 0.0174532925199433 * he(i)
        p = 0.0174532925199433 * pi(i)
        r = 0.0174532925199433 * ro(i)
        CH(i) = cos(h)
        SH(i) = sin(h)
        CP(i) = cos(p)
        SP(i) = sin(p)
        CR(i) = cos(r)
        SR(i) = sin(r)
        ea(i) = st(i) * ( CH(i) * CR(i) + SH(i) * SP(i) * SR(i) ) + 
     &          fo(i) * ( SH(i) * CP(i) ) + 
     &          ma(i) * ( CH(i) * SR(i) - SH(i) * SP(i) * CR(i) )
        no(i) = st(i) * (-SH(i) * CR(i) + CH(i) * SP(i) * SR(i) ) +
     &          fo(i) * ( CH(i) * CP(i) ) +
     &          ma(i) * (-SH(i) * SR(i) - CH(i) * SP(i) * CR(i) )
        up(i) = st(i) * (-CP(i) * SR(i) ) +
     &          fo(i) * ( SP(i) ) +
     &          ma(i) * ( CP(i) * CR(i) )
        IF (0 .EQ. MOD(n, 1000)) CALL rchkusr()
      ENDDO
      RETURN
      END
