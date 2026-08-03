# ============================================================
#  TAREA: Variable Aleatoria Gaussiana — CÓDIGO OPTIMIZADO
# ============================================================

# Colores
CF  <- rgb(0.21,0.54,0.87,0.35); CF2 <- rgb(0.87,0.27,0.27,0.35)CL  <- "#185FA5"; CR <- "#C0392B"; CG <- "#27AE60"
CP  <- "#8E44AD"; CO <- "#E67E22"; CT <- "#2C3E50"; CB <- "#BDC3C7"

# ── Función campana base ──────────────────────────────────────
camp <- function(mu, sigma, a, b, ca=CF, xm=4, xl="x", tit="") {
  xlim <- c(mu-xm*sigma, mu+xm*sigma)
  x <- seq(xlim[1], xlim[2], l=800); y <- dnorm(x,mu,sigma)
  plot(x, y, type="l", lwd=3, col=CL, xlim=xlim, ylim=c(0,max(y)*1.30),
       xlab=xl, ylab="Densidad f(x)", main=tit, axes=FALSE,
       col.main=CT, font.main=2, cex.main=1.05)
  xs <- seq(max(a,xlim[1]), min(b,xlim[2]), l=500)
  polygon(c(xs[1],xs,tail(xs,1)), c(0,dnorm(xs,mu,sigma),0), col=ca, border=NA)
  axis(1,col=CB,col.axis=CT,cex.axis=0.9); axis(2,col=CB,col.axis=CT,cex.axis=0.85,las=1)
  box(col=CB); abline(v=mu, lty=2, col=CB, lwd=1.2)
}

# ── Función resultado al pie ──────────────────────────────────
res <- function(txt) mtext(txt, side=1, line=4.5, col="#1A5276", cex=0.80, font=2)

# ── Función para sombra extra ─────────────────────────────────
sombra <- function(mu, sigma, desde, hasta, col) {
  xs <- seq(desde, hasta, l=300)
  polygon(c(xs[1],xs,tail(xs,1)), c(0,dnorm(xs,mu,sigma),0), col=col, border=NA)
}

# ============================================================
par(mar=c(7,5,4,3), family="sans")

# ============================================================
# EJERCICIO 1  —  X ~ N(70, 5²) °C
# ============================================================
mu1<-70; s1<-5; li1<-mu1-3*s1; ls1<-mu1+3*s1
z82 <- (82-mu1)/s1;  p82 <- pnorm(82,mu1,s1,lower.tail=FALSE)

cat("═══ EJ1(a) ═══\n")
cat(sprintf("[μ-3σ, μ+3σ] = [%.0f, %.0f] °C  →  0.27%% anomalías\n\n", li1, ls1))
camp(mu1,s1,li1,ls1, tit="Ej1(a) — Sensor IoT: X~N(70,5²)\nIntervalo normal [μ±3σ] = [55, 85] °C",
     xl="Temperatura (°C)")
sombra(mu1,s1, mu1-4.5*s1, li1, CF2); sombra(mu1,s1, ls1, mu1+4.5*s1, CF2)
abline(v=c(li1,ls1),col=CR,lty=2,lwd=2)
ymax<-dnorm(mu1,mu1,s1)
text(mu1,ymax*1.15,"μ=70°C",col=CT,cex=0.9,font=2)
text(li1,ymax*0.09,"55°C",col=CR,cex=0.88,pos=2,font=2)
text(ls1,ymax*0.09,"85°C",col=CR,cex=0.88,pos=4,font=2)
text(mu1,ymax*0.52,"NORMAL\n99.73%",col=CL,cex=0.95,font=2)
legend("topright",bty="n",cex=0.85,legend=c("Zona NORMAL (99.73%)","Zona ANOMALÍA (0.27%)"),fill=c(CF,CF2),border=c(CL,CR))
res("RESULTADO: Intervalo normal=[55,85]°C  |  Solo 0.27% de lecturas son anomalías  |  27 alertas por cada 10,000 lecturas")

cat("═══ EJ1(b) ═══\n")
cat(sprintf("Z=(82-70)/5=%.2f  |  P(X>82)=%.4f (%.2f%%)  |  NO dispara alerta\n\n", z82,p82,p82*100))
camp(mu1,s1,82,mu1+4.5*s1, ca=rgb(0.15,0.65,0.30,0.4),
     tit=sprintf("Ej1(b) — Sensor IoT: X~N(70,5²)\nP(X>82°C) = %.4f   Z=(82-70)/5=%.2f",p82,z82),
     xl="Temperatura (°C)")
abline(v=82,col=CG,lwd=2.5); abline(v=ls1,col=CR,lty=2,lwd=1.8)
text(82,dnorm(82,mu1,s1)+ymax*0.12,sprintf("82°C\nZ=%.2f",z82),col=CG,cex=0.90,font=2,pos=4)
text(mu1+3.2*s1,ymax*0.45,sprintf("P=%.4f\n=%.2f%%",p82,p82*100),col=CG,cex=0.90,font=2)
legend("topright",bty="n",cex=0.88,legend=sprintf("P(X>82°C)=%.4f (%.2f%%)",p82,p82*100),fill=rgb(0.15,0.65,0.30,0.4),border=CG)
res(sprintf("RESULTADO: Z=2.40  |  P=0.0082 (0.82%%)  |  82°C está DENTRO de [55,85]. NO es anomalía, pero es valor inusual"))

cat("═══ EJ1(c) ═══\n")
cat("Regla empírica: μ±1σ→31.73% anomalía | μ±2σ→4.55% | μ±3σ→0.27%\n\n")
vals<-c(100-68.27,100-95.45,100-99.73)
bp<-barplot(vals,names.arg=c("μ±1σ\n[65,75]°C","μ±2σ\n[60,80]°C","μ±3σ\n[55,85]°C"),
            col=c("#E74C3C","#C0392B","#7B241C"),ylim=c(0,max(vals)*1.4),
            main="Ej1(c) — Sensor IoT: X~N(70,5²)\n% de anomalías por intervalo (Regla Empírica 68-95-99.7)",
            ylab="% lecturas anómalas",border="white",axes=FALSE,col.main=CT,font.main=2,cex.main=1.05)
axis(2,las=1,col=CB,col.axis=CT,cex.axis=0.88)
text(bp,vals+0.8,paste0(vals,"%"),cex=1.0,font=2,col=CT); abline(h=0,col=CB)
legend("topright",bty="n",cex=0.88,legend="Recomendado: μ±3σ → 0.27% alertas",fill="#7B241C",border="white")
res("RESULTADO: Con μ±3σ=[55,85]°C → 0.27% de lecturas son anomalías = 27 alertas por cada 10,000 lecturas")

# ============================================================
# EJERCICIO 2  —  X ~ N(650, 80²)
# ============================================================
mu2<-650; s2<-80; z550<-(550-mu2)/s2; p550<-pnorm(550,mu2,s2); u5<-mu2+qnorm(0.05)*s2

cat("═══ EJ2(a) ═══\n")
cat(sprintf("Z=(550-650)/80=%.4f  |  550 está 1.25σ a la izquierda de μ\n\n", z550))
camp(mu2,s2,mu2-4*s2,550, ca=CF2, xm=3.8, xl="Puntuación crediticia",
     tit=sprintf("Ej2(a) — Crédito: X~N(650,80²)\nEstandarización umbral 550  →  Z=(550-650)/80=%.2f",z550))
abline(v=550,col=CR,lwd=2.5)
ymax2<-dnorm(mu2,mu2,s2)
text(mu2,ymax2*1.15,"μ=650",col=CT,cex=0.9,font=2)
text(550,ymax2*0.65,sprintf("550pts\nZ=%.2f",z550),col=CR,cex=0.90,font=2,pos=2)
legend("topright",bty="n",cex=0.88,legend=sprintf("Z(550)=%.2f",z550),fill=CF2,border=CR)
res(sprintf("RESULTADO: Z=-1.25  |  El umbral 550 está 1.25σ a la IZQUIERDA de μ=650  |  Cliente significativamente por debajo del promedio"))

cat("═══ EJ2(b) ═══\n")
cat(sprintf("P(X<550)=P(Z<-1.25)=%.4f  →  %.2f%% de alto riesgo\n\n", p550,p550*100))
camp(mu2,s2,mu2-4*s2,550, ca=CF2, xm=3.8, xl="Puntuación crediticia",
     tit=sprintf("Ej2(b) — Crédito: X~N(650,80²)\nP(X<550)=P(Z<%.2f)=%.4f  →  %.2f%% alto riesgo",z550,p550,p550*100))
abline(v=550,col=CR,lwd=2.5)
text(mu2,ymax2*1.15,"μ=650",col=CT,cex=0.9,font=2)
text(mu2-2.8*s2,ymax2*0.55,sprintf("P=%.4f\n=%.2f%%",p550,p550*100),col=CR,cex=0.92,font=2)
legend("topright",bty="n",cex=0.88,legend=sprintf("P(X<550)=%.4f → %.2f%% alto riesgo",p550,p550*100),fill=CF2,border=CR)
res(sprintf("RESULTADO: P(X<550)=0.1056 → 10.56%% de clientes es ALTO RIESGO  |  ~11 de cada 100 clientes bajo el umbral"))

cat("═══ EJ2(c) ═══\n")
cat(sprintf("Z=-1.6449 → Umbral=650+(-1.6449×80)=%.0f pts para 5%% riesgo\n\n", u5))
camp(mu2,s2,mu2-4*s2,u5, ca=rgb(0.55,0.27,0.67,0.35), xm=3.8, xl="Puntuación crediticia",
     tit=sprintf("Ej2(c) — Crédito: X~N(650,80²)\nUmbral para 5%% riesgo  →  T=650+(-1.645×80)=%.0f pts",u5))
sombra(mu2,s2,mu2-4*s2,550,CF2)
abline(v=u5,col=CP,lwd=2.5,lty=2); abline(v=550,col=CR,lwd=2.5)
text(mu2,ymax2*1.15,"μ=650",col=CT,cex=0.9,font=2)
text(u5, ymax2*0.55,sprintf("NUEVO\n%.0f pts\n→5%%",u5),col=CP,cex=0.88,font=2,pos=2)
text(550,ymax2*0.80,sprintf("Actual\n550pts\n→%.1f%%",p550*100),col=CR,cex=0.88,font=2,pos=4)
legend("topright",bty="n",cex=0.88,
       legend=c(sprintf("Actual 550 pts → %.2f%%",p550*100),sprintf("Propuesto %.0f pts → 5%%",u5)),
       fill=c(CF2,rgb(0.55,0.27,0.67,0.35)),border=c(CR,CP))
res(sprintf("RESULTADO: Z=-1.6449 → Umbral=%.0f pts  |  Bajando de 550 a %.0f se controla el alto riesgo al 5%% exacto",u5,u5))

# ============================================================
# EJERCICIO 3  —  ε ~ N(0, 200²)
# ============================================================
mu3<-0; s3<-200; s3b<-120
p_in<-2*pnorm(400,mu3,s3,lower.tail=FALSE)
p_mej<-2*pnorm(400,mu3,s3b,lower.tail=FALSE)
esp30<-30*p_in

cat("═══ EJ3(a) ═══\n")
cat(sprintf("Z=400/200=%.2f  |  P(|ε|>400)=2×%.4f=%.4f (%.2f%%)\n\n", 400/s3,p_in/2,p_in,p_in*100))
camp(mu3,s3,mu3-4.2*s3,mu3+4.2*s3, xm=4.2, xl="Error de predicción (unidades)",
     tit=sprintf("Ej3(a) — Errores: ε~N(0,200²)\nP(|ε|>400)=2×P(ε>400)  Z=400/200=%.2f  P=%.4f",400/s3,p_in))
for(seg in list(c(400,920),c(-920,-400))) sombra(mu3,s3,seg[1],seg[2],CF2)
abline(v=c(-400,400),col=CR,lty=2,lwd=2)
ymax3<-dnorm(mu3,mu3,s3)
text(c(-400,400),ymax3*1.12,c("-400","+400"),col=CR,cex=0.92,font=2)
text(c(-680,680),ymax3*0.55,sprintf("%.2f%%",p_in*100/2),col=CR,cex=0.95,font=2)
text(mu3,ymax3*0.55,"ACEPTABLE\n95.45%",col=CL,cex=0.90,font=2)
legend("topright",bty="n",cex=0.88,legend=c("Aceptable |ε|≤400",sprintf("Inaceptable |ε|>400 → %.2f%%",p_in*100)),fill=c(CF,CF2),border=c(CL,CR))
res(sprintf("RESULTADO: Z=2.00  |  P(|ε|>400)=0.0455 → 4.55%% inaceptable  |  2.27%% en cada cola"))

cat("═══ EJ3(b) ═══\n")
cat(sprintf("E[inaceptables]=30×%.4f=%.2f  →  ~1 o 2 errores/mes\n\n", p_in,esp30))
dias<-1:30; acum<-cumsum(rep(p_in,30))
plot(dias,acum,type="l",lwd=3,col=CL,
     main=sprintf("Ej3(b) — Errores: ε~N(0,200²)\nErrores inaceptables acumulados en 30 días  →  Total≈%.2f",esp30),
     xlab="Día del mes",ylab="Errores acumulados",ylim=c(0,esp30*1.5),
     axes=FALSE,col.main=CT,font.main=2,cex.main=1.05)
axis(1,at=c(1,5,10,15,20,25,30),col=CB,col.axis=CT,cex.axis=0.9)
axis(2,col=CB,col.axis=CT,cex.axis=0.88,las=1); box(col=CB)
abline(h=esp30,col=CR,lty=2,lwd=2); abline(h=1,col=CG,lty=2,lwd=1.8)
points(30,esp30,pch=19,col=CR,cex=1.8)
text(20,esp30+0.07,sprintf("%.2f errores/mes",esp30),col=CR,cex=0.90,font=2)
text(15,esp30*0.50,sprintf("30 × %.4f = %.2f",p_in,esp30),col=CL,cex=0.88,font=2)
legend("topleft",bty="n",cex=0.88,legend=c("Acumulado esperado",sprintf("Total≈%.2f al mes",esp30)),col=c(CL,CR),lwd=c(3,2),lty=c(1,2))
res(sprintf("RESULTADO: E[inaceptables]=30×0.0455=%.2f  →  Se esperan ~1 o 2 predicciones inaceptables por mes",esp30))

cat("═══ EJ3(c) ═══\n")
cat(sprintf("σ=120: Z=%.4f  P=%.6f (%.4f%%)  →  reducción %.1f%%\n\n", 400/s3b,p_mej,p_mej*100,(1-p_mej/p_in)*100))
xr<-seq(-750,750,l=900)
plot(xr,dnorm(xr,mu3,s3),type="l",lwd=3,col=CL,
     ylim=c(0,max(dnorm(xr,mu3,s3b))*1.30),
     main=sprintf("Ej3(c) — Comparación σ=200 vs σ=120\nP inaceptable: %.2f%% → %.4f%%  |  Reducción %.1f%%",p_in*100,p_mej*100,(1-p_mej/p_in)*100),
     xlab="Error (unidades)",ylab="Densidad f(ε)",axes=FALSE,col.main=CT,font.main=2,cex.main=1.05)
lines(xr,dnorm(xr,mu3,s3b),lwd=3,col=CO,lty=2)
axis(1,col=CB,col.axis=CT,cex.axis=0.9); axis(2,col=CB,col.axis=CT,cex.axis=0.85,las=1); box(col=CB)
for(seg in list(c(400,750),c(-750,-400))) sombra(mu3,s3,seg[1],seg[2],CF2)
abline(v=c(-400,400),col=CR,lty=2,lwd=2)
ymax3b<-max(dnorm(xr,mu3,s3b))
text(0,ymax3b*0.80,"σ=120\nmejorado",col=CO,cex=0.88,font=2)
text(0,dnorm(0,mu3,s3)*0.40,"σ=200\noriginal",col=CL,cex=0.82,font=2)
legend("topright",bty="n",cex=0.88,
       legend=c(sprintf("σ=200 → P=%.2f%%",p_in*100),sprintf("σ=120 → P=%.4f%%",p_mej*100),"Zona inaceptable"),
       col=c(CL,CO,NA),lwd=c(3,3,NA),lty=c(1,2,NA),fill=c(NA,NA,CF2),border=c(NA,NA,CR))
res(sprintf("RESULTADO: σ=200→4.55%%  |  σ=120→0.0858%%  |  Reducción 98.1%%  |  Campana más angosta elimina casi todos los errores"))

# ============================================================
# EJERCICIO 4  —  X ~ N(34, 9²)
# ============================================================
mu4<-34; s4<-9
z25<-(25-mu4)/s4; z43<-(43-mu4)/s4; p_r<-pnorm(43,mu4,s4)-pnorm(25,mu4,s4)
z55<-(55-mu4)/s4; p55<-pnorm(55,mu4,s4,lower.tail=FALSE)
p90<-mu4+qnorm(0.90)*s4

cat("═══ EJ4(a) ═══\n")
cat(sprintf("Z(25)=%.2f  Z(43)=%.2f  P(25≤X≤43)=%.4f → %.2f%%\n\n", z25,z43,p_r,p_r*100))
camp(mu4,s4,25,43, xm=3.8, xl="Edad (años)",
     tit=sprintf("Ej4(a) — Streaming: X~N(34,9²)\nP(25≤X≤43)=P(%.2f≤Z≤%.2f)=%.4f → %.2f%%",z25,z43,p_r,p_r*100))
abline(v=c(25,43),col=CL,lty=2,lwd=2)
ymax4<-dnorm(mu4,mu4,s4)
text(mu4,ymax4*1.15,"μ=34 años",col=CT,cex=0.9,font=2)
text(25,ymax4*0.09,"25 años\n(Z=-1)",col=CL,cex=0.82,pos=2,font=2)
text(43,ymax4*0.09,"43 años\n(Z=+1)",col=CL,cex=0.82,pos=4,font=2)
text(mu4,ymax4*0.58,sprintf("%.2f%%\nde usuarios",p_r*100),col=CL,cex=1.0,font=2)
legend("topright",bty="n",cex=0.88,legend=sprintf("P(25≤X≤43)=%.4f → %.2f%%",p_r,p_r*100),fill=CF,border=CL)
res(sprintf("RESULTADO: Z(25)=-1  Z(43)=+1  |  P=0.6827 → 68.27%% de usuarios  |  Corresponde a μ±1σ (regla empírica)"))

cat("═══ EJ4(b) ═══\n")
cat(sprintf("Z(55)=(55-34)/9=%.4f > 2  →  ATÍPICO  |  P(X>55)=%.4f (%.2f%%)\n\n", z55,p55,p55*100))
camp(mu4,s4,55,mu4+4.5*s4, ca=CF2, xm=3.8, xl="Edad (años)",
     tit=sprintf("Ej4(b) — Streaming: X~N(34,9²)\nUsuario 55 años: Z=(55-34)/9=%.2f → ¿Atípico?",z55))
abline(v=55,col=CR,lwd=2.5); abline(v=mu4+2*s4,col=CB,lty=3,lwd=1.5)
text(mu4,ymax4*1.15,"μ=34 años",col=CT,cex=0.9,font=2)
text(mu4+2*s4,ymax4*0.18,"μ+2σ=52",col=CB,cex=0.78,pos=4)
text(55,dnorm(55,mu4,s4)+ymax4*0.12,sprintf("55 años\nZ=%.2f\n→ATÍPICO",z55),col=CR,cex=0.88,font=2,pos=4)
text(mu4+3.5*s4,ymax4*0.52,sprintf("P=%.4f\n=%.2f%%",p55,p55*100),col=CR,cex=0.90,font=2)
legend("topright",bty="n",cex=0.88,legend=sprintf("P(X>55)=%.4f (%.2f%%) → ATÍPICO",p55,p55*100),fill=CF2,border=CR)
res(sprintf("RESULTADO: Z=2.33>2 → VALOR ATÍPICO  |  P(X>55)=0.0098 → solo 0.98%% de usuarios supera los 55 años"))

cat("═══ EJ4(c) ═══\n")
cat(sprintf("Z=1.2816 → Edad=34+(1.2816×9)=%.1f años  →  10%% más mayor\n\n", p90))
camp(mu4,s4,p90,mu4+4.5*s4, ca=rgb(0.55,0.27,0.67,0.35), xm=3.8, xl="Edad (años)",
     tit=sprintf("Ej4(c) — Streaming: X~N(34,9²)\nPercentil 90: Edad=34+(1.28×9)=%.1f años → 10%% más mayor",p90))
abline(v=p90,col=CP,lwd=2.5,lty=2)
text(mu4,ymax4*1.15,"μ=34 años",col=CT,cex=0.9,font=2)
text(p90,ymax4*0.65,sprintf("P90\n≈%.0f años",p90),col=CP,cex=0.92,font=2,pos=4)
text(mu4+3.5*s4,ymax4*0.50,"10%\nmás\nmayor",col=CP,cex=0.90,font=2)
legend("topright",bty="n",cex=0.88,legend=sprintf("10%% más mayor: >%.0f años",p90),fill=rgb(0.55,0.27,0.67,0.35),border=CP)
res(sprintf("RESULTADO: Z=1.28 → Edad=%.1f años  |  El 10%% de usuarios con mayor edad supera los %.0f años",p90,p90))

# ============================================================
# EJERCICIO 5  —  X ~ N(120, 25²)
# ============================================================
mu5<-120; s5<-25; s5p<-sqrt(2*s5^2)
z170<-(170-mu5)/s5; p170<-pnorm(170,mu5,s5,lower.tail=FALSE)
T_n<-mu5+qnorm(0.99)*s5; T_p<-mu5+qnorm(0.99)*s5p

cat("═══ EJ5(a) ═══\n")
cat(sprintf("Z=(170-120)/25=%.2f  |  P(X>170)=%.4f (%.2f%%)  →  poco frecuente\n\n", z170,p170,p170*100))
camp(mu5,s5,170,mu5+4.5*s5, ca=rgb(0.15,0.65,0.30,0.4), xm=4.2, xl="Tiempo de respuesta (ms)",
     tit=sprintf("Ej5(a) — SLA Web: X~N(120,25²) ms\nP(X>170ms)=%.4f   Z=(170-120)/25=%.2f",p170,z170))
abline(v=170,col=CG,lwd=2.5)
abline(v=c(mu5+2*s5,mu5+3*s5),col=CB,lty=3,lwd=1.5)
ymax5<-dnorm(mu5,mu5,s5)
text(mu5,ymax5*1.15,"μ=120ms",col=CT,cex=0.9,font=2)
text(170,dnorm(170,mu5,s5)+ymax5*0.12,sprintf("170ms\nZ=%.2f",z170),col=CG,cex=0.88,font=2,pos=4)
text(mu5+3.5*s5,ymax5*0.52,sprintf("P=%.4f\n=%.2f%%",p170,p170*100),col=CG,cex=0.90,font=2)
legend("topright",bty="n",cex=0.88,legend=sprintf("P(X>170ms)=%.4f (%.2f%%)",p170,p170*100),fill=rgb(0.15,0.65,0.30,0.4),border=CG)
res(sprintf("RESULTADO: Z=2.00  |  P=0.0228 (2.28%%)  |  Ocurre ~1/44 solicitudes. POCO FRECUENTE pero no extremo (no llega a μ+3σ=195ms)"))

cat("═══ EJ5(b) ═══\n")
cat(sprintf("Z=2.326 → T=120+(2.326×25)=%.2f ms para SLA 99%%\n\n", T_n))
camp(mu5,s5,mu5-4*s5,T_n, xm=4.2, xl="Tiempo de respuesta (ms)",
     tit=sprintf("Ej5(b) — SLA Web: X~N(120,25²) ms\nUmbral T para SLA 99%%:  T=120+(2.326×25)=%.0f ms",T_n))
abline(v=T_n,col=CL,lwd=2.5,lty=2)
text(mu5,ymax5*1.15,"μ=120ms",col=CT,cex=0.9,font=2)
text(T_n,ymax5*0.65,sprintf("T=%.0fms\n99%% bajo\neste umbral",T_n),col=CL,cex=0.88,font=2,pos=2)
text(mu5-2.5*s5,ymax5*0.50,"99%\ndentro\ndel SLA",col=CL,cex=0.90,font=2)
legend("topright",bty="n",cex=0.88,legend=sprintf("T SLA 99%% = %.0f ms",T_n),fill=CF,border=CL)
res(sprintf("RESULTADO: Z=2.326 → T=%.0f ms  |  El sistema debe responder en <%.0f ms para cumplir SLA al 99%%",T_n,T_n))

cat("═══ EJ5(c) ═══\n")
cat(sprintf("σ_pico=√1250=%.2f  →  T_pico=%.2f ms  (+%.0f ms vs normal)\n\n", s5p,T_p,T_p-T_n))
xr5<-seq(mu5-4.5*s5p,mu5+4.5*s5p,l=900)
plot(xr5,dnorm(xr5,mu5,s5),type="l",lwd=3,col=CL,
     ylim=c(0,max(dnorm(xr5,mu5,s5))*1.30),
     main=sprintf("Ej5(c) — SLA Web: normal vs tráfico pico\nT normal=%.0fms  →  T pico=%.0fms  (+%.0f ms adicionales)",T_n,T_p,T_p-T_n),
     xlab="Tiempo de respuesta (ms)",ylab="Densidad f(x)",axes=FALSE,col.main=CT,font.main=2,cex.main=1.05)
lines(xr5,dnorm(xr5,mu5,s5p),lwd=3,col=CO,lty=2)
axis(1,col=CB,col.axis=CT,cex.axis=0.9); axis(2,col=CB,col.axis=CT,cex.axis=0.85,las=1); box(col=CB)
abline(v=mu5,col=CB,lty=3,lwd=1.2)
abline(v=T_n,col=CL,lwd=2.5,lty=2); abline(v=T_p,col=CO,lwd=2.5,lty=2)
ymax5<-max(dnorm(xr5,mu5,s5))
text(mu5,ymax5*1.15,"μ=120ms",col=CT,cex=0.9,font=2)
text(T_n,ymax5*0.80,sprintf("Normal\nT=%.0fms",T_n),col=CL,cex=0.88,font=2,pos=2)
text(T_p,ymax5*0.55,sprintf("Pico\nT=%.0fms",T_p),col=CO,cex=0.88,font=2,pos=4)
text(mu5,ymax5*0.42,sprintf("+%.0f ms\nen pico",T_p-T_n),col=CR,cex=0.88,font=2)
legend("topright",bty="n",cex=0.88,
       legend=c(sprintf("Normal σ=25 → T=%.0fms",T_n),sprintf("Pico σ=√1250 → T=%.0fms",T_p)),
       col=c(CL,CO),lwd=3,lty=c(1,2))
res(sprintf("RESULTADO: σ_pico=%.1fms → T_pico=%.0fms  |  En tráfico pico el SLA requiere %.0f ms más de margen",s5p,T_p,T_p-T_n))


