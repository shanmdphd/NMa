<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:template match="/">
    <html>
      <body>
        <center>
        <h3 style="font-size: 20pt"> 
          Summary of NONMEM OUT Files </h3>
        <table border="1" >
          <tr style="font-size: 10pt; font-family: tahoma; font-weight: bold; background-color: aqua">
            <td style="text-align: center">Number</td>            
            <td style="text-align: center">Out Name</td>
            <td style="text-align: center">Parent</td>
            <td style="text-align: center">Formula</td>
            <td style="text-align: center">Minimize</td>
            <td style="text-align: center">Obj Fx Value</td>
            <td style="text-align: center">Std Error</td>
            <td style="text-align: center">Parameters</td>
            <td style="text-align: center">Thetas</td>
            <td style="text-align: center">Fixed Thetas</td>
            <td style="text-align: center">Etas</td>
            <td style="text-align: center">Eps</td>
            <td style="text-align: center">Off Diag Etas</td>
            <td style="text-align: center">Last Run</td>
          </tr>
          <xsl:for-each select="outputs/output">
            <xsl:sort select="lorder" />
            <tr style="font-size: 10pt; font-family: verdana">
              <xsl:if test="position() mod 2 = 0">
                <xsl:attribute name="bgcolor">yellow</xsl:attribute>
              </xsl:if>
              <td style="text-align: center"><xsl:value-of select="lorder"/></td>
              <td style="text-align: center"><xsl:value-of select="outname"/></td>
              <td style="text-align: center"><xsl:value-of select="parent"/></td>
              <td style="text-align: center"><xsl:value-of select="formula"/></td>
              <td style="text-align: center"><xsl:value-of select="minimize"/></td>
              <td style="text-align: center"><xsl:value-of select="ofv"/></td>
              
              <td style="text-align: center">
			          <xsl:if test="se = 'True'">
			            <xsl:attribute name="style">
				            <xsl:text>color:red; font-weight:bold; text-align: center</xsl:text>
			            </xsl:attribute>
			          </xsl:if>
                <xsl:value-of select="se"/>
              </td>

              <td style="text-align: center"><xsl:value-of select="parameters"/></td>
              <td style="text-align: center"><xsl:value-of select="thetas"/></td>
              <td style="text-align: center"><xsl:value-of select="fixthetas"/></td>
              <td style="text-align: center"><xsl:value-of select="etas"/></td>
              <td style="text-align: center"><xsl:value-of select="eps"/></td>
              <td style="text-align: center"><xsl:value-of select="offdiagetas"/></td>
              <td style="text-align: center"><xsl:value-of select="lastwrite"/></td>
            </tr>
          </xsl:for-each>
        </table>
        </center>
      </body>
    </html>
  </xsl:template>
</xsl:stylesheet>
