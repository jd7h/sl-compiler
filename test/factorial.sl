Int facR ( Int n )
{	if ( n < 2 )
		return 1;
	else
		return n * facR ( n - 1 );
}

