
Hierarchical Linear Model (HLM)

\\[ \log \left( \mathsf{Loss}\_{ijklm} \right) = \alpha\_{1} t + \alpha\_{2ijkl} \log \left( \mathsf{Production}\_{ijklm} + 1 \right) + \mathsf{A}\_{ijklm} \\]

\\[ \alpha\_{2ijkl} = \beta\_{20} + \beta\_{2ijk} \left( \mathsf{Country:Commodity} \right)\_{ijkl} + \mathsf{B}\_{ijkl} \\]

\\[ \beta\_{2ijk} = \gamma\_{20} + \gamma\_{2ij} \left( \mathsf{Commodity} \right) + \mathsf{C}\_ijk \\]

\\[ \gamma\_{2ij} = \delta\_{20} + \delta\_{2i} \left( \mathsf{Food Group} \right) + D\_{ij} \\]

\\[ \delta\_{2i} = \zeta\_{20} + \zeta\_{21} \left( \mathsf{Food Perishable Group}\_{i} \right) + \mathsf{E}\_{i} \\]
