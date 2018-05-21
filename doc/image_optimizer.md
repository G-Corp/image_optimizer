

# Module image_optimizer #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

__This module defines the `image_optimizer` behaviour.__<br /> Required callback functions: `optimize/3`.

<a name="types"></a>

## Data Types ##




### <a name="type-option">option()</a> ###


<pre><code>
option() = {quality, integer()} | {quiet, true | false} | {strip_metadata, true | false} | {output, <a href="file.md#type-filename_all">file:filename_all()</a>}
</code></pre>




### <a name="type-options">options()</a> ###


<pre><code>
options() = [<a href="#type-option">option()</a>]
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#optimize-1">optimize/1</a></td><td>Equivalent to <a href="#optimize-2"><tt>optimize(File, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#optimize-2">optimize/2</a></td><td>
Optimize an image file.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="optimize-1"></a>

### optimize/1 ###

`optimize(File) -> any()`

Equivalent to [`optimize(File, [])`](#optimize-2).

<a name="optimize-2"></a>

### optimize/2 ###

<pre><code>
optimize(File::<a href="file.md#type-filename_all">file:filename_all()</a>, Options::<a href="#type-options">options()</a>) -&gt; {ok, <a href="file.md#type-filename_all">file:filename_all()</a>} | {error, term()} | {error, integer(), string()}
</code></pre>
<br />

Optimize an image file

