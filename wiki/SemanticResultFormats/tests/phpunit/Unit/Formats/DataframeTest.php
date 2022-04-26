<?php

namespace SRF\Tests\Dataframe;

use SMW\Test\QueryPrinterRegistryTestCase;

class DataframeTest extends QueryPrinterRegistryTestCase{

	/**
	 * @see QueryPrinterRegistryTestCase::getFormats
	 *
	 * @since 3.2
	 *
	 * @return array
	 */
	public function getFormats() {
		return [ 'dataframe' ];
	}

	/**
	 * @see QueryPrinterRegistryTestCase::getClass
	 *
	 * @since 3.2
	 *
	 * @return string
	 */
	public function getClass() {
		return '\SRF\Dataframe\DataframePrinter';
	}
}