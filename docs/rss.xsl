<?xml version="1.0" encoding="UTF-8"?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:param name="quote">"</xsl:param>
  <xsl:template match="/rss">
    <html>
      <head>
        <style>
          <xsl:text>
            body {
                margin: 0 auto;
                max-width: 800px;
                display: flex;
                flex-direction: column;
                color: #e0e0e0;
                background-color: #333333;
                text-align: center;
                font-size: max(2vh, 10pt);
            }
            h2 { color: #f6f6f6; }
            a { text-decoration: none; }
            a:visited { color: #9f85ff; }
            table {
                text-align: center;
                border-collapse: collapse;
                font-size: max(1.8vh, 9pt);
            }
            tr:nth-child(even) { background-color: #2b2b2b; }
            th, td { border-bottom: 1px solid #444444; }
            .blogpost-entry:hover {
                color: #f6f6f6;
                background-color: #161718;
                transition: color 0.2s;
            }
          </xsl:text>
        </style>
      </head>
      <body>
        <h2>Lens_r's Blogposts</h2>
        <table>
          <tr>
            <th>Title</th>
            <th>Publish Date</th>
          </tr>
          <xsl:for-each select="channel/item">
            <xsl:element name="tr">
              <xsl:attribute name="id">blogpost-entry</xsl:attribute>
              <td>
                <xsl:element name="a">
                  <xsl:attribute name="href"><xsl:value-of select="link"/></xsl:attribute>
                  <xsl:value-of select="title"/>
                </xsl:element>
              </td>
              <td><xsl:value-of select="pubDate"/></td>
            </xsl:element>
          </xsl:for-each>
        </table>
      </body>
    </html>
  </xsl:template>
</xsl:stylesheet>
