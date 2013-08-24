SELECT hc.numvertices, hc.numedges, IFNULL(numfirstwins, 0), IFNULL(numsecondwins, 0), IFNULL(numneitherwins, 0)
FROM (SELECT numvertices, numedges FROM hypergraphs GROUP BY numvertices, numedges) hc
LEFT OUTER JOIN 
     (SELECT numvertices, numedges, COUNT(*) AS numfirstwins
      FROM hypergraphs NATURAL JOIN results_Perfect_vs_Perfect
      WHERE winner = "First" GROUP BY numvertices, numedges) fwc
ON (hc.numvertices = fwc.numvertices AND hc.numedges = fwc.numedges)
LEFT OUTER JOIN 
     (SELECT numvertices, numedges, COUNT(*) AS numsecondwins
      FROM hypergraphs NATURAL JOIN results_Perfect_vs_Perfect
      WHERE winner = "Second" GROUP BY numvertices, numedges) swc
ON (hc.numvertices = swc.numvertices AND hc.numedges = swc.numedges)
LEFT OUTER JOIN
     (SELECT numvertices, numedges, COUNT(*) AS numneitherwins
      FROM hypergraphs NATURAL JOIN results_Perfect_vs_Perfect
      WHERE winner = "Neither" GROUP BY numvertices, numedges) nwc
ON (hc.numvertices = nwc.numvertices AND hc.numedges = nwc.numedges);
