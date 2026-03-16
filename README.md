# Coursework1_221117152_Time-Series

## Project description
Time series analysis and model for web search history on "exercise" using R and Meta Prophet

<h1 class="title toc-ignore">Coursework1_221117152</h1>

</div>


<p>As someone that runs and exercises a lot in my free time, I wanted to
understand people’s interest in exercising and whether it has improved
over the years. My time series is taken from Google Trends and gives
information on the indexed volume of web search history for the term
“Exercise” in GB from 2011 to 2025. First, plot the time series and
model it using simple linear regression.</p>
<pre class="r"><code>gb_exercise &lt;- read.csv(&quot;C:/Users/rache/OneDrive/Desktop/Year 4/Sem 2 - Time series/Coursework1_221117152/GB_exercise_searchHistory.csv&quot;)
gb_exercise &lt;- ts(gb_exercise$exercise, start = c(2011, 1), frequency = 12)
