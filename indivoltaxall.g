new ,60000;
ttt = time;
ddd = date;
cls;
call sysstate(14,1e-128);

@--------------------------------------------------------------------------------------------------------------------@
@------------------------A variety of flags to set-------------------------------------------------------------------@
@--------------------------------------------------------------------------------------------------------------------@

numlag  = 5;                 @ The number of lags for the Newey West stuff.@
altmpk  = 0;                 @ This changes the specification of mpk. 0 is the cobb-douglass version and 1 is the  mark-up version. @
altinv = 0;                  @ This changes the definition of investment@
netcap = 0;                  @ This changes the deflating variable @
levon = 1;                   @ This looks at levered versus unlevered returns @
imax = 2;                    @ This is the largest power in the adjustment cost function. @
fixcost = 0;                 @ This is a flag that allows for an extra parameter called "fixcost" (whatever) in the investment return. @
optwait = 0;                 @ This is a flag to use multi-step GMM @
lagit = 1;                   @ A flag that is 0 if the investment return is contemporaneous and 1 if it is ahead. @
raw = 0;                     @ A flag that is 1 for using raw second moments and zero for volatilities.@
parsedet = 0;                @ This flag sets the d/a ratio equal across all the observations in the portfolio. @
tau_cfix= 0.35;              @ Corporate tax rate @
tshield = 1;                 @ Do we account for the depreciation tax shield.@
stupidbond = 0;              @ Is the bond rate adjusted for taxes (0) or not (1).@
stupidtax  = 0;              @ Is the tax rate portfolio specific (1) or not (0).@


@ Step 1: ------Name of the Gauss data set.----------------------------------@

    in1 = "c:\\00res\\expret\\dta08a\\allports";

@ Step 2: ------Name of the output file.-------------------------------------@


@ Step 4: -------Set the maximum number of GMM rounds.----------------------@

    if optwait == 0;
    output file = indivol1tax.out reset;
    rlim = 1;
    else;
    rlim = 10;
    output file = indivol2tax.out reset;
    endif;

@ Step 5: -------Set the maximum number of iterations.----------------------@

    maxiter = 400;

@ Step 6: -----------------Set the limit on the number of squeezes.---------@

    maxsqez=40;

@ Step 7: -----------------Set the tolerance on the minimization routine.---@

    tol=1e-5;

@--------Open GAUSS data set. -------------------------------------@

    outwidth  256;
    closeall f1;
    open f1=^in1;


alldta = readr(f1,rowsf(f1));

numl =0;
do while numl <= 2;


ew = 0;
do while ew <= 1;


altinv = 0;
do while altinv <= 1;

netcap = 0;
do while netcap <=1;

tauc = 0;
do while tauc <=1;


tshield = 0;
do while tshield <=1;


if numl == 1 and ew == 1 and netcap == 0 and tauc == 1 and tshield == 1 and altinv == 1;

"****************************************************************************************************************************************";
"########################################################################################################################################";
"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!";
print; print;
"This is the specification used in the paper.";
print;print;
"****************************************************************************************************************************************";
"########################################################################################################################################";
"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!";
endif;


if numl == 0;     print;print;print;print;"Window length = 1"; numlag = 1;
elseif numl == 1; print;print;print;print;"Window length = 5"; numlag = 5;
elseif numl == 2; print;print;print;print;"Window length = 10"; numlag = 10;
endif;

if ew == 1; "Equal weighted returns";
elseif ew == 0; "Value weighted returns";
endif;
if altinv==0; "Laura's I (what is in the appendix)";
elseif altinv==1; "Laura's I2 (what is in the paper)";
endif;
if netcap == 0; "Gross Capital Stock";
elseif netcap == 1; "Net Capital Stock";
endif;
if tauc == 0; "Constant tax rate";
elseif tauc == 1; "Time Varying tax rate";
endif;
if tshield == 0; "No depreciation tax shield";
elseif tshield == 1; "Depreciation tax shield";
endif;


let pname = GROUP;
let pid  = PID;
alltypes = alldta[.,indcv(pname,getname(in1))];
let tname = FF       EM33    EM      IGR  BM    I2K      IND   MKT;
let allns = 25       9       10      10   10    10       9   1;

@------------------------------------This is where the group loop starts.-------------------------------------------@

for tt(3,5,1);  @ This is the portfolio group loop.@
     $tname[tt,1];

     let pid  = PID;
     tttt = tt;
     x = selif(alldta,alltypes.$==tname[tt,1]);
     obnum = x[.,indcv("YEAR",getname(in1))]-1962;
     x = selif(alldta,alltypes.$==tname[tt,1]);
     nnn = minc(x[.,indcv(pid,getname(in1))]);
     xxx = maxc(x[.,indcv(pid,getname(in1))]);


    if ew == 0;



       if altinv == 0;
           if netcap == 1;
              let iretvars =  fsale_nk fi_nk sale_lnk i_lnk dep_lnk vwbndret d_ma cost_lnk fcost_nk vwret ftaxrate eviltax;
           else;
              let iretvars =  fsale_gk fi_gk sale_lgk i_lgk dep_lgk vwbndret d_ma cost_lgk fcost_gk vwret ftaxrate eviltax;
           endif;

       elseif altinv == 1;
           if netcap == 1;
              let iretvars =  fsale_nk fi2_nk sale_lnk i2_lnk dep_lnk vwbndret d_ma cost_lnk fcost_nk vwret ftaxrate eviltax;
           else;
              let iretvars =  fsale_gk fi2_gk sale_lgk i2_lgk dep_lgk vwbndret d_ma cost_lgk fcost_gk vwret ftaxrate eviltax;
           endif;
       endif;



     elseif ew == 1;



       if altinv == 0;
           if netcap == 1;
              let iretvars =  fsale_nk fi_nk sale_lnk i_lnk dep_lnk ewbndret d_ma cost_lnk fcost_nk ewret ftaxrate eviltax;
           else;
              let iretvars =  fsale_gk fi_gk sale_lgk i_lgk dep_lgk ewbndret d_ma cost_lgk fcost_gk ewret ftaxrate eviltax;
           endif;

       elseif altinv == 1;
           if netcap == 1;
              let iretvars =  fsale_nk fi2_nk sale_lnk i2_lnk dep_lnk ewbndret d_ma cost_lnk fcost_nk ewret ftaxrate eviltax;
           else;
              let iretvars =  fsale_gk fi2_gk sale_lgk i2_lgk dep_lgk ewbndret d_ma cost_lgk fcost_gk ewret ftaxrate eviltax;
           endif;
       endif;

     endif;

     obnum = x[.,indcv("YEAR",getname(in1))]-1962;

     if lagit == 1;
       lx    = x[1:rows(x)-1,.];
       x     = x[2:rows(x),.];

       pick1 = (obnum[2:rows(obnum),1]-obnum[1:rows(obnum)-1,1]).==1;
       x = selif(x,pick1);
       lx = selif(lx,pick1);
       obnum = selif(obnum[2:rows(obnum),1],pick1);  @ minc(x[.,4]~obnum); goto rrrr; @
     endif;

     year     = x[.,indcv("YEAR",getname(in1))];
     irets    = x[.,indcv(iretvars,getname(in1))];
     pid      = x[.,indcv(pid,getname(in1))];
     pdum     = x[.,indcv(pname,getname(in1))];


     fsales   = irets[.,1];
     fik      = irets[.,2];
     sales    = irets[.,3];
     ik       = irets[.,4];
     drate    = irets[.,5];
     baar     = irets[.,6];
     darat    = irets[.,7];
     costs    = irets[.,8];
     fcosts   = irets[.,9];
     ret      = irets[.,10];
     tau_c    = irets[.,11];
     if stupidtax == 0; tau_c = irets[.,12]; endif;

     if lagit == 1;

       lirets    = lx[.,indcv(iretvars,getname(in1))];
       lfsales   = lirets[.,1];
       lfik      = lirets[.,2];
       lsales    = lirets[.,3];
       lik       = lirets[.,4];
       ldrate    = lirets[.,5];
       lbaar     = lirets[.,6];
       ldarat    = lirets[.,7];
       lcosts    = lirets[.,8];
       lfcosts   = lirets[.,9];
       lret      = lirets[.,10];
       ltau_c    = lirets[.,11];
       if stupidtax == 0; ltau_c = lirets[.,12]; endif;

     else;

       sales = fsales;
       lik   = ik;
       ik    = fik;

     endif;


     z = ones(rows(irets),1)@~lffret~lopinc~lik~lsue@;

     /*-------------------------This part parses out the depreciation rate by portfolio.------------------*/

     clear dr;
     mip = minc(pid);
     map = maxc(pid);
     for mm(mip,map,1);
     dri = selif(drate,pid.==mm);   if dri==miss(1,1);"shit";endif;
     dr = dr|(meanc(dri)*ones(rows(dri),1));
     endfor;
     drate = dr[2:rows(dr),1];

     /*-------------------------This part parses out the debt assets ratio by portfolio.------------------*/

     if parsedet == 1;

       clear dr;
       mip = minc(pid);
       map = maxc(pid);
       for mm(mip,map,1);
       dri = selif(darat,pid.==mm);   if dri==miss(1,1);"shit";endif;
       dr = dr|(meanc(dri)*ones(rows(dri),1));
       endfor;
       darat = dr[2:rows(dr),1];

     endif;

     clear testret;
     clear wsave;
     cls;
     ii = imax;
     do while ii>=imax;

          neq = cols(z)*allns[tt,1]*2;
          nc = ii+fixcost;
          if neq<=nc; "Not overidentified!";end;endif;
          n = rows(irets)/allns[tt,1];

          @---------- Here we input the starting values.----------------@

          c = zeros(nc,1);
          c = ones(nc,1);
          c = 0.23*ones(nc,1);
          let cname = a1 a2 a3 a4;
          cname = cname[1:ii-1,1]|"ks"|"fc";


          rr = 1;
          do until rr > rlim;

                if rr == 1;
                  w = eye(neq);
                else;
                  if optwait;
                    isdeff=0;
                    ww = deff(c);
                    w=invit(ww);
                    if w==-999999;
                       w = invpd(diagrv(zeros(neq,neq),diag(ww))); tt;;"shit";
                    endif;
                  endif;
                endif;

                @ ------------- Start of iteration loop ------------------------@
                iter = 1;
                dc=1;                     @ Initialize the step length. @
                gwf=1;
                do until abs(dc) < tol;

                    isdeff=1;
                    f=deff(c);                @ The program jumps to the subroutine that
                                               defines the f vector. @

                    g=grad(&deff,c);          @ The program jumps to the subroutine that
                                                computes analytic derivatives. The matrix
                                                of partials of f with respect to the
                                                parameters is called g. @

                    obj = f'w*f;              @ This computes the value of the objective
                                                function.@

                    gwg= g'w*g;               @ This uses the GAUSS-NEWTON method
                                                to compute full step dc. @
                    gwf = g'w*f;

                    dc = solvit(gwf,gwg);

                    if maxsqez > 0;              @ This jumps to the subroutine that
                                                   adjusts the step length. @
                      { c_new,sqz } = squeeze(c,dc);
                    else;
                      c_new=c - dc;
                    endif;


                    dc=c_new-c;                     @ Update variables for the next iteration. @
                    c=c_new;


                    /*
                    /*--------------Print out the results for the current iteration------------*/
                    output off;
                    locate 2,2;
                    "Round Number       = ";; rr;
                    "Iteration Number   = ";; iter;
                    "Number of Squeezes = ";; sqz;
                    "Objective Function = ";; obj*n;

                    "                                  Value           Step                Gradient";
                     format /rd 14,6;
                     mm=1;
                     do until mm > (rows(c));
                      $cname[mm,1];;"  ";; c[mm,1];; "  ";; dc[mm,1];;  "  ";; gwf[mm,1];
                      mm=mm+1;
                      endo;

                    output on;


                    /*-----------------End of print out of current iteration--------------*/
                    */
                     iter=iter +1;
                     if iter >= maxiter or key==27 or abs(c)>2000;                     @ Quit iterating if necessary. @
                       goto escape;
                     endif;
                endo;
                @ ------ End of iteration loop ------------------------------------ @
                escape:

                if rr == rlim;

                     @ Compute t-ratios, etc.                                            @

                     isdeff=1;
                     f = deff(c);
                     g=grad(&deff,c);

                     outmx = ret~(testret-1);

                     isdeff=0;
                     sw = deff(c);
                     gwg = g'w*g;
                     if rlim == 1;
                       vc=invpd(gwg)*g'w*sw*w*g*invpd(gwg)/n;
                     else;
                       vc=g'*invit(sw)*g/n;
                     endif;

                     stderr=sqrt(diag(vc));
                     if rlim > 1;
                       chisq = f'invit(sw)*f*n;
                       degf  = neq - nc;
                       pval  = cdfchic(chisq,degf);
                     else;
                       chisq = f'w*f*n;
                       degf  = neq - nc;
                       pval  = cdfchic(chisq,degf);
                     endif;

                     if ii==imax;
                       firstchi=chisq;
                     else;
                       lstat = chisq - firstchi;
                       df = imax - ii;
                       pv = cdfchic(lstat,df);
                     endif;


                     iks = (1/2*ik^2)~(1/3*ik^3)~(1/4*ik^4)~(1/4*ik^5);
                     iks = iks[.,1:ii-1];
                     adjcosts = meanc(iks*c[1:ii-1,1]);
                     @"              & " adjcosts;; " \\" "\\";
                     @iks = (ik)~(ik^2)~(ik^3)~(ik^4);
                     iks = iks[.,1:ii-1];
                     adjcosts = meanc(iks*c[1:ii-1,1]);
                     @"              & " adjcosts;; " \\" "\\";
                     print;
                     @
                     format /rd 12,8;
                     stockret = zeros(allns[tt,1],1);
                     stockstd = zeros(allns[tt,1],1);
                     dair = zeros(allns[tt,1],1);
                     baair = zeros(allns[tt,1],1);
                     ikret = zeros(allns[tt,1],1);
                     testret = testret - 1;
                     for jj(nnn,xxx,1);
                       idx = jj;
                       stockret[jj-nnn+1,1] = meanc(selif(ret,pid.==idx));
                       stockstd[jj-nnn+1,1] = stdc(selif(ret,pid.==idx));
                       dair[jj-nnn+1,1] = meanc(selif(darat,pid.==idx));
                       baair[jj-nnn+1,1] = meanc(selif(baar,pid.==idx));
                       ikret[jj-nnn+1,1] = meanc(selif(testret,pid.==idx));
                     endfor;

                     "--------------------------------------------------------------------------------------------------------";
                     "Stock returns and Investment returns ";
                     "--------------------------------------------------------------------------------------------------------";
                     "       Stock   Investment ";
                     stockret~ikret;
                     print;
                      @
                     format /rd 8,2;
                     stockret~ikret;
                     print;
                       @
                     icept = ones(rows(stockret),1);
                     des = icept~ikret;
                     @
                     slope = invpd(moment(des,0))*des'stockret;
                     "Slope     " slope[2,1];
                     "Intercept " slope[1,1];
                     @
                     vpe = 1/n*(eye(neq) - g*invpd(gwg)*g'w)*sw*(eye(neq)-g*invpd(gwg)*g'w)';



                     @-----------------------One form of a pseudo-inverse--------------Gauss has a better one
                     { va,ve } = eigv(vpe);
                     va = (eig(vpe));
                     ve = (ve);
                     va = 0.*(abs(va).<1e-8) + va.*(abs(va).>=1e-8);
                     va = 0.*(va.==0) + (1./(va + (va.==0))).*(va./=0);
                     va = diagrv(eye(rows(va)),va);
                     ivpe = ve*va*ve';
                     @
                     "Pricing errors and standard errors.";

                     format /rd 12,8;
                     "    Error        Std.         T-stat. ";
                     sdvpe = sqrt(diag(vpe));
                     f[1:rows(f)/2,1]~sdvpe[1:rows(f)/2,1]~(f[1:rows(f)/2,1]./sdvpe[1:rows(f)/2,1]);
                     print;

                     "High minus low ";


                     hml    = -(f[1,1]-f[0.5*rows(f),1]);
                     stdhml = sqrt(vpe[1,1]+vpe[0.5*rows(vpe),0.5*rows(vpe)]-2*vpe[1,0.5*rows(vpe)]);
                     thml   = -(f[1]-f[0.5*rows(f)])/sqrt(vpe[1,1]+vpe[0.5*rows(vpe),0.5*rows(vpe)]-2*vpe[1,0.5*rows(vpe)]);
                     hml;; stdhml;; thml;

                      print;print;

                      tstatf = (f./sdvpe);

                     "----------------------------------------------------------Table 3 Format ------------------------------------------------";
                     format /rd 6,2;

                     " & ";; for aaa(1,rows(f)/2,1); 100*f[aaa,1];;      " & ";; endfor; 100*hml;; "  \\" "\\";
                     " &[";; for aaa(1,rows(f)/2,1); tstatf[aaa,1];; "]&[";; endfor; thml;; "] \\" "\\";

                     print;print;print; "--------------------------------Now the same stuff for volatilities------------------------------------";
                                        "-------------------------------------------------------------------------------------------------------";
                     print;
                     format /rd 12,8;
                     "Actual and Implied volatilities ";
                     "       Actual  Implied";
                     stockstd~(stockstd-f[rows(f)/2+1:rows(f),1]);


                     "Pricing errors and standard errors.";

                     format /rd 12,8;
                     "    Error        Std.         T-stat. ";
                     sdvpe = sqrt(diag(vpe));
                     f[rows(f)/2+1:rows(f),1]~sdvpe[rows(f)/2+1:rows(f),1]~(f[rows(f)/2+1:rows(f),1]./sdvpe[rows(f)/2+1:rows(f),1]);
                     print;


                     "High minus low ";

                     hilo = f[0.5*rows(f)+1,1]-f[rows(f),1];
                     svhilo = sqrt(vpe[0.5*rows(vpe)+1,0.5*rows(vpe)+1]+vpe[rows(vpe),rows(vpe)]-2*vpe[0.5*rows(vpe)+1,rows(vpe)]);
                     thilo = hilo./svhilo;
                     hilo;; svhilo;;thilo;
                     print;print;


                     "----------------------------------------------------------Table 3 Format ------------------------------------------------";
                     format /rd 6,2;

                     " & ";; for aaa(rows(f)/2+1,rows(f),1); 100*f[aaa,1];;      " & ";; endfor; 100*hilo;; "  \\" "\\";
                     " &[";; for aaa(rows(f)/2+1,rows(f),1); tstatf[aaa,1];; "]&[";; endfor; thilo;; "] \\" "\\";


                    /*
                     gee = zeros(rows(f)/2,rows(f));
                     for ggg(1,rows(f)/2,1);
                       gee[ggg,ggg] = -f[ggg,1]/2;
                       gee[ggg,rows(f)/2+ggg] = 1;
                     endfor;
                    @ gee = (0.5/(sqrt(f[rows(f)/2+1:rows(f),1])-f[1:rows(f)/2,1]^2)).*gee;     @



                     vvol = gee*vpe*gee';
                     vvol = sqrt(diag(vvol));
                     mvol = f[rows(f)/2+1:rows(f)]-f[1:rows(f)/2,1]^2;
                     @mvol = sqrt(mvol); @
                     @mvol~sqrt(diag(vvol));end; @
                     */

                     format /rd 8,4;
                     ivpe = invswp(vpe);
                     chisq = f'ivpe*f;   if chisq<0; chisq = 0;endif;
                     pval = cdfchic(chisq,degf);

                     pe2   = f[1:rows(f)/2,1];
                     ivpe2 = invswp(vpe[1:rows(f)/2,1:rows(f)/2]);
                     chisq2 = f[1:rows(f)/2,1]'ivpe2*f[1:rows(f)/2,1];
                     degf2 = neq/2-nc;
                     pval2 = cdfchic(chisq2,degf2);

                     pe3   = f[rows(f)/2+1:rows(f),1];
                     ivpe3 = invswp(vpe[rows(f)/2+1:rows(f),rows(f)/2+1:rows(f)]);
                     chisq3 = f[rows(f)/2+1:rows(f),1]'ivpe3*f[rows(f)/2+1:rows(f),1];
                     degf3 = neq/2-nc;
                     pval3 = cdfchic(chisq3,degf3);
                     @
                     "Average Absolute Pricing Error"; print;
                     @
                     "Decimal per year " meanc(abs(f[1:rows(f)/2,1]));       @
                     "Percent per month" meanc(abs(f[1:rows(f)/2,1]))/12*100;
                     print;      @
                     @"Joint test that the pricing errors are zero"; print;
                     "& ";; chisq;; " &";
                     "&(";; pval;;  ")&";
                     @
                     /*   This business makes a LaTeX table.   */    print;print; "-------------------------------------";

                     format /rd 12,3;
                     for qq(1,ii+fixcost,1);
                     format /rd 6,3;
                     $cname[qq,1] " &  ";; c[qq,1];;       " \\" "\\";
                     format /rd 6,2;
                     "        &[";; stderr[qq,1];;  "] \\" "\\";
                     endfor;
                     @"              & " adjcosts;; " \\" "\\";@

                     format /rd 6,3;
                     "        & ";; chisq3;; "  \\" "\\";
                     format /rd 6,0;
                     "        & ";; degf3;; "  \\" "\\";
                     format /rd 6,3;
                     "        & ";; pval3;;  "  \\" "\\";
                     "        & ";; meanc(abs(100*f[rows(f)/2+1:rows(f),1]));; "  \\" "\\";

                     "        & ";; chisq2;; "  \\" "\\";
                     format /rd 6,0;
                     "        & ";; degf2;; "  \\" "\\";
                     format /rd 6,3;
                     "        & ";; pval2;;  "  \\" "\\";
                     "        & ";; meanc(100*abs(f[1:rows(f)/2,1]));; "  \\" "\\";


                     "        & ";; chisq;; "  \\" "\\";
                     format /rd 6,0;
                     "        & ";; degf;; "  \\" "\\";
                     format /rd 6,3;
                     "        & ";; pval;;  "  \\" "\\";



                     if ii<imax;
                     "              & ";; lstat;; "  \\" "\\";
                     "              & ";; df  ;; "  \\" "\\";
                     "              &(";; pv  ;;  ") \\" "\\";
                     print;
                     endif;
                     "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@";
                     "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@";
                     "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@";
                     "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@";
                     /*
                     "Actual volatilities and implied volatilities";
                     print;

                     for mmmm(1,allns[tt,1],1);
                         actvol  = stdc(outmx[(mmmm-1)*n+1:mmmm*n,1]);
                         impvol1 = selif(outmx[(mmmm-1)*n+1:mmmm*n,2],abs(outmx[(mmmm-1)*n+1:mmmm*n,2]).<1);
                         impvol1 = stdc(impvol1);
                         impvol2 = stdc(outmx[(mmmm-1)*n+1:mmmm*n,2]);
                         meanc(outmx[(mmmm-1)*n+1:mmmm*n,1]);; " & ";; actvol;; " & ";; impvol2;; " & ";@; impvol1;; " & ";; mvol[mmmm,1];;  " & ";; vvol[mmmm,1];; " & ";; mvol[mmmm,1]/vvol[mmmm,1]; @
                     endfor;
                     */

                endif;


          rr = rr + 1;
          endo;


     ii = ii - 1;
     endo;

     rrrr:

endfor; @End of the portfolio loop.@

tshield = tshield + 1; endo;
tauc = tauc + 1; endo;
netcap = netcap + 1; endo;
altinv = altinv + 1; endo;
ew = ew + 1; endo;
numl = numl + 1; endo;

@ ----------------- Subroutines follow --------------- @

@ Subroutine to define the f vector. @

proc deff(c);

     local a0,a1,a2,a3,a4,jj,qq,idx,u,zi,salesi,iki,liki,dratei,darati,reti,f,ireti,dumi,baai,fc,ls,fi,monthi,costi,yeari,
     mpk, ireti1, ireti2, vv, teste,tau_ci, ff, nn;

     testret=0;

     a1     = c[1,1];
     if ii>=3;
     a2     = c[2,1];
     endif;
     if ii>=4;
     a3     = c[3,1];
     endif;
     if ii==5;
     a4     = c[4,1];
     endif;
     ls     = c[ii,1];
     if fixcost == 1;
     fc     = c[ii+1,1];
     else;
     fc = 0;
     endif;


     for vv(1,2,1);

         for qq(nnn,xxx,1);

           idx = qq;
           costi  = selif(costs,pid.==idx);
           salesi = selif(sales,pid.==idx);
           iki    = selif(ik,pid.==idx);
           liki   = selif(lik,pid.==idx);
           baai   = selif(baar,pid.==idx);
           dumi   = selif(pdum,pid.==idx);
           zi     = selif(z,pid.==idx);
           reti   = selif(ret,pid.==idx);
           dratei = selif(drate,pid.==idx);
           darati = selif(darat,pid.==idx);
           yeari  = selif(year,pid.==idx);
           tau_ci = selif(tau_c,pid.==idx);

           if tauc == 0;


             if altmpk == 1;
               mpk = (salesi - ls*costi).*(1-tau_cfix);
             elseif altmpk == 0;
               mpk = ls*salesi.*(1-tau_cfix)-fc;
             endif;
             if stupidbond == 0;

               baai = 1 + baai;
               baai = tau_cfix + (1 - tau_cfix)*baai;
               baai = baai - 1;

             endif;


           elseif tauc == 1;

             if altmpk == 1;
               mpk = (salesi - ls*costi).*(1-tau_ci);
             elseif altmpk == 0;
               mpk = ls*salesi.*(1-tau_ci)-fc;
             endif;
             if stupidbond == 0;
               baai = 1 + baai;
               baai = tau_ci + (1 - tau_ci).*baai;
               baai = baai - 1;
             endif;


           endif;
           if ii==5;
                        ireti1 = ( mpk + a1/2*iki^2 + 2*a2/3*iki^3 + 3*a3/4*iki^4  + 4*a4/5*iki^5 + tshield*dratei.*(tauc*tau_cfix + (1-tauc)*tau_ci) + (1 - dratei).*(1 + a1*iki^1 + a2*iki^2 + a3*iki^3 + a4*iki^4));
                        ireti2 = (1 + a1*liki^1 + a2*liki^2 + a3*liki^3 + a4*liki^4);
           elseif ii==4;
                        ireti1 = ( mpk + a1/2*iki^2 + 2*a2/3*iki^3 + 3*a3/4*iki^4                 + tshield*dratei.*(tauc*tau_cfix + (1-tauc)*tau_ci) + (1 - dratei).*(1 + a1*iki^1 + a2*iki^2 + a3*iki^3));
                        ireti2 = (1 + a1*liki^1 + a2*liki^2 + a3*liki^3);
           elseif ii==3;
                        ireti1 = ( mpk + a1/2*iki^2 + 2*a2/3*iki^3                                + tshield*dratei.*(tauc*tau_cfix + (1-tauc)*tau_ci) + (1 - dratei).*(1 + a1*iki^1 + a2*iki^2));
                        ireti2 = (1 + a1*liki^1 + a2*liki^2);
           elseif ii==2;
                        ireti1 = ( mpk + (a1/2*iki^2).*(1 - tauc*tau_cfix + (1-tauc)*tau_ci)                                               + tshield*dratei.*(tauc*tau_cfix + (1-tauc)*tau_ci) + (1 - dratei).*(1 + (a1*iki^1).*(1 - tauc*tau_cfix + (1-tauc)*tau_ci)));
                        ireti2 = (1 + (a1*liki^1).*(1 - tauc*tau_cfix + (1-tauc)*tau_ci));
           endif;

           u     = (darati.*(baai+1) + (1-darati).*(reti+1)) - ireti1./ireti2;
           ireti = levon*((ireti1./ireti2 - darati.*(1+baai))./(1-darati)) + (1-levon)*(ireti1./ireti2);


           @reti  = 1 + reti;     @

           ireti = ireti - 1;
           u     = reti - ireti;


             if vv == 1;

               if isdeff == 1;
                 if qq==nnn;
                   f = meanc(zi.*u);     testret=(levon*((ireti1./ireti2 - darati.*(1+baai))./(1-darati)) + (1-levon)*ireti1./ireti2);
                 elseif qq>nnn;
                   f = f|meanc(zi.*u);   testret=testret|(levon*((ireti1./ireti2 - darati.*(1+baai))./(1-darati)) + (1-levon)*ireti1./ireti2);
                 endif;

               elseif isdeff==0;

                 if qq==nnn;
                   f = zi.*u;            testret=(levon*((ireti1./ireti2 - darati.*(1+baai))./(1-darati)) + (1-levon)*ireti1./ireti2);
                 elseif qq>nnn;
                   f = f~(zi.*u);        testret=testret|(levon*((ireti1./ireti2 - darati.*(1+baai))./(1-darati)) + (1-levon)*ireti1./ireti2);
                 endif;

               endif;

             elseif vv==2;

               if isdeff == 1;
                   if raw == 1;
                     f = f|(meanc((zi.*((reti)^2 - ireti^2))));
                   else;
@                     f = f|(meanc(zi.*((reti - meanc(reti))^2 - (ireti - meanc(ireti)^2))));@
                     f = f|(meanc(zi.*(  sqrt((reti - meanc(reti))^2) - sqrt((ireti - meanc(ireti))^2)  )));
                   endif;
               elseif isdeff==0;
                   if raw == 1;
                     f = f~      ((zi.*((reti)^2 - ireti^2)));
                   else;
                   @ f = f~        zi.*((reti - meanc(reti))^2 - (ireti - meanc(ireti))^2);@
                    f = f~        zi.*(sqrt((reti - meanc(reti))^2) - sqrt((ireti - meanc(ireti))^2));
                   endif;
               endif;

             endif;
         endfor;

     endfor;

     if isdeff==1;

       retp(f@-meanc(f)@);

       clear f;
     else;

        ff = moment(f,0)/n;
        for nn(1,numlag,1);

        ff = ff + 2*(1-(nn/(numlag+1)))*(f[nn+1:rows(f),.]'f[1:rows(f)-nn,.]/(n-nn));

        endfor;
       retp(ff@moment(f,0)/n@);

       clear f;
     endif;
endp;

@  ------------------------------------------------------- @
@ Subroutine to compute gradient matrix. @

proc 1 = grad(f,x0);
    local f:proc;
    local n,k,grdd,dh,ax0,xdh,arg,dax0,i,f0;

    /* check for complex input */
    if iscplx(x0);
        if hasimag(x0);
            errorlog "ERROR: Not implemented for complex matrices.";
            end;
        else;
            x0 = real(x0);
        endif;
    endif;

    f0 = f(x0);
    n = rows(f0);
    k = rows(x0);
    grdd = zeros(n,k);

/* Computation of stepsize (dh) for gradient */

    ax0 = abs(x0);
    if x0 /= 0;
        dax0 = x0./ax0;
    else;
        dax0 = 1;
    endif;
    dh = (1e-8)*maxc((ax0~(1e-2)*ones(rows(x0),1))').*dax0;
    xdh = x0+dh;
    dh = xdh-x0;    /* This increases precision slightly */
    arg = diagrv(reshape(x0,k,k)',xdh);

    i = 1;
    do until i > k;
        grdd[.,i] = f(arg[.,i]);
        i = i+1;
    endo;

    grdd = (grdd-f0)./(dh');

    retp(grdd);
endp;


@  ------------------------------------------------------- @
@ Subroutine to compute squeezes.

  This subroutine compares the values of the objective function at the
  points c+s*dc and c+0.5*s*dc, with dc = the proposed change in the vector
  of parameters, and with step length s initially at 1. s is halved until
  minus the objective function stops declining.
@
proc (2) = squeeze(s_c,s_dc);
local s_c1,s_lm,s_itr,lc1,s_c2,lc2,s_f1,s_f2;


    s_c1=s_c - s_dc; s_lm=1/2; s_itr=1;
      isdeff=1;
      s_f1 = deff(s_c1);
    lc1 = s_f1'w*s_f1;
    clear s_f1;
  do until s_itr > maxsqez;

    s_c2=s_c-s_lm*s_dc;
      isdeff=1;
      s_f2 = deff(s_c2);
    lc2 = s_f2'w*s_f2;
    clear s_f2;

    if lc1 <= lc2 and lc1 <= obj;

       retp(s_c1,s_itr-1); goto eoproc;

    else;

       s_c1=s_c2; s_lm=s_lm/2; lc1=lc2;
       s_itr=s_itr+1;

    endif;

  endo;
retp(s_c2,s_itr-1);

eoproc:
endp;


@----------Subroutine to trap inversion errors-----------------@

proc invit(w);
local onemore,ww;
onemore = 1;
winv:
trap 1;
  ww = invpd(w);
trap 0;
  if scalerr(ww) > 0;
      if onemore == 1;
        call sysstate(14,1e-128);
        onemore = 0;
        goto winv;
      else;
        ww = -999999;
      endif;
  endif;

retp(ww);

endp;

@----------Subroutine to trap solving errors-----------------@

proc solvit(a,b);
local dc,onemore;
    onemore = 1;
    solveit:
    trap 1;
    dc = solpd(a,b);
    trap 0;
    if scalerr(dc) > 0;
      if onemore == 1;
        call sysstate(14,1e-128);
        onemore = 0;
        goto solveit;
      else;
        dc = -999999;
      endif;
    endif;

retp(dc);
endp;

@------------------------------------------------------------------------@

eop:

"Start time:  ";; timestr(ttt);
"End time:    ";; timestr(0);
"Start date:  ";; datestr(ddd);
"End date:    ";; datestr(0);

output off;
end;

