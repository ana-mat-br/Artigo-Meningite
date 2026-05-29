# DECISÃO EDITORIAL — RBE

**Manuscrito:** *Dinâmica espaçotemporal e preditores de mortalidade por meningite: análise bayesiana, LISA e regressão de Poisson modificada na região do Triângulo Mineiro, Brasil (2015–2025)*

**Arquivo avaliado:** `MENINGITE ESCRITA FINAL.docx`

**Revista-alvo:** Revista Brasileira de Epidemiologia (RBE / SciELO)

**Skill:** `academic-research-skills:academic-paper-reviewer` v1.9.0 — modo `full`

**Painel:** EIC (RBE) + R1 Metodologia + R2 Domínio + R3 Perspectiva + Advogado do Diabo

**Data:** 2026-05-29

---

## Recomendação

**REVISÃO MAIOR (Major Revision)**

**Justificativa:** O Advogado do Diabo levantou **3 questões CRÍTICAS** (IRON RULE: decisão não pode ser "Accept"). EIC, R1, R2, R3 e DA convergem em major revision — não há voto de rejeição nem de aceitação.

---

## Consenso entre os 5 revisores (5/5 concordam)

1. **Centralidade do achado de captação nos polos** — todos veem como a contribuição mais forte e original do artigo (deve virar a mensagem central).
2. **Sobreinterpretação etiologia-específica com 90,4% missing** — R2, R3, DA e EIC flagam: vacinas conjugadas, sazonalidade viral, NPI bacteriana são mecanismos invocados sobre dataset etiologia-cega.
3. **Inconsistências numéricas no resumo** — EIC, R1 e DA: SMR 1,85 com IC 1,73–1,86 é matematicamente implausível; texto x Tabela 2 sobre fase intra-pandêmica para óbito.
4. **2025 incompleto + estrutura etária do Censo 2022 fixada para 11 anos** — R1 e DA: viesa SMR e tendências.

---

## Divergência arbitrada

| Tópico | Posições | Arbitragem |
|---|---|---|
| EB antes de Moran/LISA | R1 e DA dizem que enviesa; R2/R3/EIC não comentam | **R1/DA têm razão técnica.** Refazer Moran/LISA sobre taxas brutas (ou SMR não-suavizado) e manter EB apenas para cartografia. |
| Validade de chamar "estudo ecológico misto" | R1 contesta; demais aceitam | **R1 tem razão.** Renomear para "análise espacial ecológica municipal + análise transversal individual". |
| Tese "artefato de captação" vs "burden urbano real" | DA aponta contradição com `ID_MN_RESI`; R3 ecoa pedindo arbitragem | **Não pode ficar implícito.** Exigir parágrafo discutindo (a) burden residencial real, (b) sensibilidade diagnóstica diferencial, (c) preenchimento incorreto de `ID_MN_RESI`. |

---

## Questões CRÍTICAS do Advogado do Diabo (bloqueadoras)

1. **Contradição residência × captação** (não pode coexistir sem teste empírico).
2. **Confusão idade × etiologia** no RR=2,45 para ≥50 anos.
3. **Suavização EB antes de Moran/LISA** enviesando I=0,0185 e o LISA.

---

# REVISION ROADMAP (priorizado)

## Prioridade 1 — Bloqueadores (devem ser resolvidos)

1. **Recalcular SMR e IC95% (Byar) e corrigir 1,85 com IC 1,73–1,86** — EIC, R1, DA. Verificar todos os ICs da Tabela 1.
2. **Refazer Moran Global e LISA sobre taxas brutas (ou SMR)**, mantendo EB apenas para mapa; reportar sensibilidade bruto vs suavizado — R1, DA.
3. **Censurar/corrigir 2025** (notification lag SINAN) e refazer SMR, tendências, Joinpoint e modelos pandêmicos — R1, DA.
4. **Substituir proporção etária fixa do Censo 2022** por projeções/interpolação anual — R1.
5. **Resolver contradição `ID_MN_RESI` ↔ 76% nos polos**: parágrafo na Discussão com as 3 hipóteses concorrentes (burden residencial real / sensibilidade diagnóstica / erro de preenchimento) e, se factível, teste empírico simples (razão SIM-residência vs SINAN-residência; cruzamento `SG_UF_NOT` × `SG_UF_RES`) — DA, R3.
6. **Reconhecer confusão idade × etiologia no RR=2,45**: limitação explícita + moderar "característica estrutural" — R2, DA.

## Prioridade 2 — Major (recomendados)

7. **Renomear desenho** para "análise espacial ecológica municipal + análise transversal individual"; deixar claro que nenhuma inferência cruza níveis — R1.
8. **Rotular desfecho como "meningite de todas as etiologias agrupadas"** desde o título/resumo; reduzir atribuições vacinais/virais/bacterianas no enquadramento — R2, DA.
9. **Alinhar resumo/texto à Tabela 2** (fase intra-pandêmica NS para óbito) — EIC.
10. **Reposicionar contribuição** em torno do artefato de captação dos polos; novo título focado em saúde pública, não em métodos — EIC, R2.
11. **Re-incorporar equidade**: reportar n e % de exclusão por raça/cor no Resultados; justificar dicotomização; trazer resultado da imputação múltipla ao corpo; análise descritiva por raça/cor — R3.
12. **Reportar 90,4% no Resultados** e benchmark contra completude do SINAN (Sáfadi 2025 e boletins SVS) — R2, R3.
13. **Temperar afirmação de "falha estrutural nacional"** — TM tem dois polos universitários atípicos; reposicionar como hipótese — R3.
14. **Qualificar recomendação de descentralização laboratorial** (LACEN-MG, GAL, PCR multiplex, viabilidade SUS) — R3.
15. **Não excluir 131 casos por raça/cor**; colapsar categorias e harmonizar com MICE do suplementar — R1.
16. **Declaração de Data/Code Availability** + trazer Joinpoint/Prais-Winsten/GAM/GEE ao texto ou suplementar visível — R1.

## Prioridade 3 — Minor

17. Tratar LISA como exploratório (sem linguagem de "cluster acionável") dado Moran Global NS — R1, DA.
18. Discutir multiplicidade fora do FDR do LISA (5–19 anos p=0,038 limítrofe) — R1.
19. Citar carga global/GBD de meningite + 2–3 estudos espaciais brasileiros de meningite — R2.
20. Refinar declaração de uso de IA com versão exata e log — DA.

---

## Próximos passos no pipeline ARS

1. **`ars-revision-coach`** (em curso) — diálogo socrático para priorizar revisão e gerar Response Letter Skeleton.
2. **Revisão do manuscrito** (autor) — aplicar roadmap, gerar v2 + Response to Reviewers.
3. **`re-review`** — verificação R&R Traceability Matrix sobre v2 antes da submissão.
