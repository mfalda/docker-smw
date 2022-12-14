## The URL paths to the logo.  Make sure you change this from the default,
## or else you'll overwrite your logo when you upgrade!
$wgLogos = [ '1x' => "$wgResourceBasePath/resources/assets/virus_135x135.png" ];

# R-Shiny server
$shinyServer = 'http://localhost:3839';

# user permissions

# Allow only sysop to edit protected pages
$wgGroupPermissions['*']['edit'] = false;
# Disable anonymous editing
$wgGroupPermissions['*']['createpage'] = false;
$wgGroupPermissions['*']['createtalk'] = false;
$wgGroupPermissions['*']['editarticles'] = false;

# Disable for users, too: by default 'user' is allowed to edit, even if '*' is not.
$wgGroupPermissions['user']['edit'] = false;
$wgGroupPermissions['user']['createpage'] = false;
$wgGroupPermissions['user']['createtalk'] = false;
$wgGroupPermissions['user']['editarticles'] = false;

$wgGroupPermissions['viewers'] = $wgGroupPermissions['autoconfirmed'];
$wgGroupPermissions['editors'] = $wgGroupPermissions['autoconfirmed'];
$wgGroupPermissions['exporters'] = $wgGroupPermissions['autoconfirmed'];
$wgGroupPermissions['guests'] = $wgGroupPermissions['autoconfirmed'];

# Only sysop and editors can edit (and guests)
$wgGroupPermissions['sysop']['edit'] = true;
$wgGroupPermissions['sysop']['editarticles'] = true;
$wgGroupPermissions['sysop']['createpage'] = true;
$wgGroupPermissions['editors']['edit'] = true;
$wgGroupPermissions['editors']['createpage'] = true;
$wgGroupPermissions['editors']['editarticles'] = true;
$wgGroupPermissions['guests']['edit'] = true;
$wgGroupPermissions['guests']['createpage'] = true;
#$wgGroupPermissions['guests']['editarticles'] = false; // protected articles

# editors can delete
$wgGroupPermissions['editors']['delete'] = true;

# Prevent new user registrations except by sysops or editors
$wgGroupPermissions['*']['createaccount'] = false;
$wgGroupPermissions['sysop']['createaccount'] = true;
$wgGroupPermissions['editors']['createaccount'] = true;

# Disable viewing except viewers and editors (and guests)
$wgGroupPermissions['*']['read'] = false;
$wgGroupPermissions['viewers']['read'] = true;
$wgGroupPermissions['editors']['read'] = true;
$wgGroupPermissions['guests']['read'] = true;

$wgWhitelistRead = [ "Main Page", "MediaWiki:Common.css", "MediaWiki:Common.js" ];

# optionally disable edit action for all users (more secure)
#$wgActions['edit'] = false;

# Enabled extensions. Most of the extensions are enabled by adding
# wfLoadExtension( 'ExtensionName' );
# to LocalSettings.php. Check specific extension documentation for more details.
# The following extensions were automatically enabled:
wfLoadExtension( 'CategoryTree' );
wfLoadExtension( 'Interwiki' );

wfLoadExtension( 'Maps' );
$GLOBALS['egMapsLeafletAvailableLayers'][]['DBNS_Virus'] = true;

wfLoadExtension( 'ParserFunctions' );
$wgPFEnableStringFunctions = true;

wfLoadExtension( 'PdfHandler' );
wfLoadExtension( 'Scribunto' );
wfLoadExtension( 'SecureLinkFixer' );
wfLoadExtension( 'SyntaxHighlight_GeSHi' );
wfLoadExtension( 'WikiEditor' );

# End of automatically generated settings.
# Add more configuration options below.

wfLoadExtension( 'AdminLinks' );
wfLoadExtension( 'DisplayTitle' );
wfLoadExtension( 'MagicNoCache' );
wfLoadExtension( 'PageForms' );
wfLoadExtension( 'Tabber' );
wfLoadExtension( 'Variables' );
wfLoadExtension( 'Widgets' );

enableSemantics( 'localhost' );

wfLoadExtension( 'SemanticACL' );
wfLoadExtension( 'SemanticDependencyUpdater' );
wfLoadExtension( 'SemanticRating' );
wfLoadExtension( 'SemanticResultFormats' );
#$GLOBALS['srfgFormats'][] = 'dataframe';
#$GLOBALS['srfgFormats'][] = 'prolog';

include_once "$IP/extensions/SemanticDrilldown/SemanticDrilldown.php";
$sdgHideCategoriesByDefault = true;

wfLoadExtension( 'PropChainsHelper' );
$pchCatLevels = [
    'Patients' => 0,
    'Positives' => 1, // second level (first chain)
    'Samplings' => 1 // second level (second chain)
];
$pchPropLevels = [
    "Uid" => [0, 0], // First (and second) chain, first level
    "Household id" => [0, 0],
    "Random home location" => [0, 0],
    "Random work location" => [0, 0],
    "Age group" => [0, 0],
    "Gender" => [0, 0],
    "Consensus" => [0, 0],
    "Fever" => [0, 0],
    "fever temperature" => [0, 0],
    "Cough" => [0, 0],
    "Sore throat" => [0, 0],
    "Malaise" => [0, 0],
    "Headache" => [0, 0],
    "Diarrhea" => [0, 0],
    "Conjunctivitis" => [0, 0],
    "Other symptoms" => [0, 0],
    "Random symptom locations" => [0, 0],
    "Symptomatic at first sampling" => [0, 0],
    "Symptomatic at follow up" => [0, 0],
    "Symptomatic" => [0, 0],
    "First sampling" => [0, 0],
    "Second sampling" => [0, 0],
    "Collected samplings" => [0, 0],
    "Positive" => [0, 0],
    "Mean random score" => [0, 0],
    "Hospitalized" => [0, 0],
    "Deceased" => [0, 0],
    "First symptoms date" => [0, 0],
    "Clinical condition at hospitalization" => [0, 0],
    "First hospitalization date" => [0, 0],
    "Discharge date" => [0, 0],
    "Death date" => [0, 0],

    "Contact" => [0, 1],
    "Type" => [0, 1],
    "Random location of contact" => [0, 1],

    "Number" => [1, 1], // Second chain, second level
    "Date" => [1, 1],
    "Interval" => [1, 1],
    "Result" => [1, 1],
    "Random score" => [1, 1],
    "Delta from last score" => [1, 1],
    "Random symptom locations at sampling" => [1, 1]
];

$pchLinkProps = [
   ['Has Patient'],
   ['Has Patient']
];

$pchDomains = [
    "Gender_of_patient" => ["M", "F"]
];

ini_set('display_errors', 0);
//$wgDebugToolbar = true;
//$wgShowExceptionDetails = true;
//$wgParserCacheType = CACHE_NONE;
//$wgCachePages = false;

function onResourceLoaderGetConfigVars( array &$vars, string $skin, Config $config )
{
    global $shinyServer;

    $vars['sServer'] = [
        'IP' => $shinyServer
    ];

    return true;
}

$wgHooks['ResourceLoaderGetConfigVars'][] = 'onResourceLoaderGetConfigVars';

