<?xml version="1.0" encoding="utf-8"?>
<!--
  xml2sexp.xsl - convert XML to S-expression Meta Language (SML)

  Usage: xsltproc xml2sexp.xsl somefile.xml > somefile.sml

  lib/sml.arc has functions for converting SML back to XML

  Copyright (c) 2010 Jeffrey D. Brennan
 
  License: http://www.opensource.org/licenses/mit-license.php
  -->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

  <xsl:output method="text"/>

  <xsl:template match="text()">
    <xsl:variable name="text" select="."/>
    <xsl:if test="normalize-space($text)">
      <xsl:call-template name="quoteString">
        <xsl:with-param name="string">
          <xsl:value-of select="$text"/>
        </xsl:with-param>
      </xsl:call-template>
    </xsl:if>
  </xsl:template>

  <xsl:template match="*"> 
    <xsl:choose>
      <xsl:when test="@*"> (<xsl:value-of select="name()"/> (@<xsl:apply-templates select="@*"/>)</xsl:when>
      <xsl:otherwise> (<xsl:value-of select="name()"/>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:apply-templates/>)
  </xsl:template>

  <xsl:template match="@*">
    <xsl:value-of select="string(' ')"/>
    <xsl:value-of select="name()"/>
    <xsl:call-template name="quoteString">
      <xsl:with-param name="string">
        <xsl:value-of select="."/>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template name="quoteString">
    <xsl:param name="string"/> "<xsl:call-template name="escapeDoubleQuote">
      <xsl:with-param name="string">
        <xsl:call-template name="escapeBackSlash">
          <xsl:with-param name="string">
            <xsl:value-of select="$string"/>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:with-param>
    </xsl:call-template>"</xsl:template>

  <xsl:template name="escapeBackSlash">
    <xsl:param name="string"/>
    <xsl:if test="contains($string, '&#x5c;')">
      <xsl:value-of select="substring-before($string, '&#x5c;')"/>\\<xsl:call-template name="escapeBackSlash">
        <xsl:with-param name="string">
          <xsl:value-of select="substring-after($string, '&#x5c;')"/>
        </xsl:with-param>
      </xsl:call-template>
    </xsl:if>
    <xsl:if test="not(contains($string, '&#x5c;'))">
      <xsl:value-of select="$string"/>
    </xsl:if>
  </xsl:template>

  <xsl:template name="escapeDoubleQuote">
    <xsl:param name="string"/>
    <xsl:if test="contains($string, '&#x22;')">
      <xsl:value-of select="substring-before($string, '&#x22;')"/>\"<xsl:call-template name="escapeDoubleQuote">
        <xsl:with-param name="string">
          <xsl:value-of select="substring-after($string, '&#x22;')"/>
        </xsl:with-param>
      </xsl:call-template>
    </xsl:if>
    <xsl:if test="not(contains($string, '&#x22;'))">
      <xsl:value-of select="$string"/>
    </xsl:if>
  </xsl:template>

  </xsl:stylesheet>
