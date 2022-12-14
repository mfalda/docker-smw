FROM mediawiki:1.35

ENV MEDIAWIKI_EXT_BRANCH REL1_35

LABEL vendor='DNS' \
      maintainer='Marco Falda <marco.falda@unipd.it>'

RUN set -x; \
    apt-get update \
 && apt-get upgrade -y \
 && apt-get install -y --no-install-recommends vim \
    libpng-dev/stable \
    git \
    libzip-dev \
    unzip

COPY --from=composer:2.1 /usr/bin/composer /usr/bin/composer

# Install composer based extensions
RUN cd /var/www/html/ \
 && composer require mediawiki/semantic-media-wiki "~3.2" --update-no-dev \
 && composer require mediawiki/semantic-result-formats "~3.2" --update-no-dev \
 && composer require mediawiki/semantic-compound-queries "~2.1" --update-no-dev \
 && composer require mediawiki/maps "~9.0" --update-no-dev

# Install extensions with a git based install

RUN git clone --depth=1 -b $MEDIAWIKI_EXT_BRANCH https://gerrit.wikimedia.org/r/mediawiki/extensions/ExternalData.git /var/www/html/extensions/ExternalData || true

RUN git clone --depth=1 -b $MEDIAWIKI_EXT_BRANCH https://gerrit.wikimedia.org/r/mediawiki/extensions/PageForms.git /var/www/html/extensions/PageForms || true

RUN git clone --depth=1 -b $MEDIAWIKI_EXT_BRANCH https://gerrit.wikimedia.org/r/mediawiki/extensions/Arrays.git /var/www/html/extensions/Arrays || true

RUN git clone --depth=1 -b $MEDIAWIKI_EXT_BRANCH https://gerrit.wikimedia.org/r/mediawiki/extensions/HierarchyBuilder.git /var/www/html/extensions/HierarchyBuilder || true

RUN git clone --depth=1 -b $MEDIAWIKI_EXT_BRANCH https://gerrit.wikimedia.org/r/mediawiki/skins/Material.git /var/www/html/skins/Material || true

RUN git clone --depth=1 -b $MEDIAWIKI_EXT_BRANCH https://gerrit.wikimedia.org/r/mediawiki/extensions/DisplayTitle.git /var/www/html/extensions/DisplayTitle || true

RUN git clone --depth=1 -b $MEDIAWIKI_EXT_BRANCH https://gerrit.wikimedia.org/r/mediawiki/extensions/MagicNoCache.git /var/www/html/extensions/MagicNoCache || true

RUN git clone --depth=1 -b $MEDIAWIKI_EXT_BRANCH https://gerrit.wikimedia.org/r/mediawiki/extensions/Tabber.git /var/www/html/extensions/Tabber || true

RUN git clone --depth=1 -b $MEDIAWIKI_EXT_BRANCH https://gerrit.wikimedia.org/r/mediawiki/extensions/Variables.git /var/www/html/extensions/Variables || true

RUN git clone --depth=1 -b $MEDIAWIKI_EXT_BRANCH https://gerrit.wikimedia.org/r/mediawiki/extensions/Widgets.git /var/www/html/extensions/Widgets && cd /var/www/html/extensions/Widgets && composer update --no-dev || true

RUN git clone --depth=1 -b $MEDIAWIKI_EXT_BRANCH https://gerrit.wikimedia.org/r/mediawiki/extensions/SemanticACL.git /var/www/html/extensions/SemanticACL || true

RUN git clone --depth=1 https://github.com/gesinn-it/SemanticDependencyUpdater.git /var/www/html/extensions/SemanticDependencyUpdater || true

RUN git clone --depth=1 -b $MEDIAWIKI_EXT_BRANCH https://gerrit.wikimedia.org/r/mediawiki/extensions/SemanticRating.git /var/www/html/extensions/SemanticRating || true

RUN git clone --depth=1 -b $MEDIAWIKI_EXT_BRANCH https://gerrit.wikimedia.org/r/mediawiki/extensions/CategoryTree.git /var/www/html/extensions/CategoryTree || true

RUN git clone --depth=1 -b $MEDIAWIKI_EXT_BRANCH https://gerrit.wikimedia.org/r/mediawiki/extensions/Interwiki.git /var/www/html/extensions/Interwiki || true

RUN git clone --depth=1 -b $MEDIAWIKI_EXT_BRANCH https://gerrit.wikimedia.org/r/mediawiki/extensions/ParserFunctions.git /var/www/html/extensions/ParserFunctions || true

RUN git clone --depth=1 -b $MEDIAWIKI_EXT_BRANCH https://gerrit.wikimedia.org/r/mediawiki/extensions/PdfHandler.git /var/www/html/extensions/PdfHandler || true

RUN git clone --depth=1 -b $MEDIAWIKI_EXT_BRANCH https://gerrit.wikimedia.org/r/mediawiki/extensions/AdminLinks.git /var/www/html/extensions/AdminLinks || true

RUN git clone --depth=1 -b $MEDIAWIKI_EXT_BRANCH https://gerrit.wikimedia.org/r/mediawiki/extensions/Scribunto.git /var/www/html/extensions/Scribunto || true

RUN git clone --depth=1 -b $MEDIAWIKI_EXT_BRANCH https://gerrit.wikimedia.org/r/mediawiki/extensions/SecureLinkFixer.git /var/www/html/extensions/SecureLinkFixer || true

RUN git clone --depth=1 -b $MEDIAWIKI_EXT_BRANCH https://gerrit.wikimedia.org/r/mediawiki/extensions/SyntaxHighlight_GeSHi.git /var/www/html/extensions/SyntaxHighlight_GeSHi || true

RUN git clone --depth=1 -b $MEDIAWIKI_EXT_BRANCH https://gerrit.wikimedia.org/r/mediawiki/extensions/WikiEditor.git /var/www/html/extensions/WikiEditor || true

RUN git clone --depth=1 https://github.com/mfalda/mediawiki-extensions-SemanticDrilldown.git /var/www/html/extensions/SemanticDrilldown || true

RUN git clone --depth=1 https://github.com/mfalda/PropChainsHelper.git /var/www/html/extensions/PropChainsHelper || true

COPY LocalSettings.local.php /var/www/html/LocalSettings.local.php

# Place icons
COPY virus_135x135.png /var/www/html/resources/assets/

# Copy maps
COPY images/WomanAnatShape/ /var/www/html/images/

# Smarty plugins for Widget extension
COPY Widgets/smarty_plugins /var/www/html/extensions/Widgets/

# Copy additional export formats
COPY SemanticResultFormats/DefaultSettings.php /var/www/html/extensions/SemanticResultFormats/
COPY SemanticResultFormats/SemanticResultFormats.php /var/www/html/extensions/SemanticResultFormats/
COPY SemanticResultFormats/i18n/en.json /var/www/html/extensions/SemanticResultFormats/i18n/
COPY SemanticResultFormats/i18n/it.json /var/www/html/extensions/SemanticResultFormats/i18n/
COPY SemanticResultFormats/i18n/qqq.json /var/www/html/extensions/SemanticResultFormats/i18n/
COPY SemanticResultFormats/formats/Prolog/PrologPrinter.php /var/www/html/extensions/SemanticResultFormats/formats/Prolog/
COPY SemanticResultFormats/formats/dataframe/DataframePrinter.php /var/www/html/extensions/SemanticResultFormats/formats/dataframe/

# add the dependency for the Spreadsheet format
RUN docker-php-ext-install zip gd
RUN composer require phpoffice/phpspreadsheet "~1" --update-no-dev
RUN composer update --no-dev

# permissions for the Widgets compiled templates
RUN chown -R www-data: /var/www/html/extensions/Widgets/compiled_templates/
RUN chmod -R a+rw /var/www/html/extensions/Widgets/compiled_templates/

# additional overlays for Maps
RUN ln -s /var/www/html/images/WomanAnatShape resources/assets/
COPY DBNS_Virus.js /var/www/html/
RUN sed -i -e "/Wikimedia: {/r /var/www/html/DBNS_Virus.js" -e "s/Wikimedia: {//" extensions/Maps/resources/lib/leaflet-providers/leaflet-providers.js

# patch for Timelines
COPY SimileTimeline/labellers.js extensions/SemanticResultFormats/formats/timeline/resources/SimileTimeline/scripts/

# patch for the new PHP
RUN sed -i "s/value != 0/value != ''/" extensions/SemanticMediaWiki/src/DataValues/Number/IntlNumberFormatter.php


# Virus data
COPY schema_Virus.xml /var/www/html/
COPY data_Virus_pos.xml /var/www/html/

#RUN php maintenance/importDump.php schema_Virus.xml
#RUN php maintenance/importDump.php data_Virus_pos.xml
## I am not able to ensure that the DB is already up here
#RUN php maintenance/update.php --quick

HEALTHCHECK CMD curl -f http://localhost:8081/ || exit 1
